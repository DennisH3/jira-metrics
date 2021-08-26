# No support for rolling dates or picking the start/end date and for how long in functions

#' Create a shiny app that functions as a GUI
#'
#' @return shinyApp
#' @export
jiraApp <- function(){

  # Define UI ---------------------------------------------------------------
  ui <- shiny::fluidPage(
    # Title of the app
    shiny::titlePanel("JIRA Metrics Data Visualization Tool"),

    # Create the tab panel
    shiny::tabsetPanel(

      shiny::tabPanel("Metric Visualizations", fluid = TRUE,

               # Sidebar layout with input and output definitions ----
               shiny::sidebarLayout(

                 # Sidebar panel for inputs ----
                 shiny::sidebarPanel(

                   # Upload CSV file
                   shiny::fileInput("file",
                             shiny::h3("Upload JIRA Data CSV or XML file"),
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv",
                                        ".xml",
                                        "text/xml")),

                   # Only show other widgets when CSV file is read
                   shiny::conditionalPanel(
                     condition = "output.fileUploaded == 'csv'",

                     # Selectize input that allows user to choose which metrics to keep
                     shiny::uiOutput("col"),

                     shiny::dateInput("endDate",
                                      shiny::h4("Pick End Date for Closed Epics")),

                     shiny::numericInput("p",
                                         shiny::h4("Last X Months for Closed Epics"),
                                  value = 12,
                                  min = 1
                     ),

                     shiny::dateInput("endDate2",
                                      shiny::h4("Pick End Date for New Issues")),

                     shiny::numericInput("p2",
                                         shiny::h4("Last X Months for New Issues"),
                                  value = 12,
                                  min = 1
                     ),

                     shiny::dateInput("endDate3",
                                      shiny::h4("Pick End Date for Active Issues")),

                     shiny::numericInput("p3",
                                         shiny::h4("Last X Months for Active Issues"),
                                  value = 12,
                                  min = 1
                     )

                   ),

                   # Conditional Panel for XML files
                   shiny::conditionalPanel(
                     condition = "output.fileUploaded == 'xml'",

                     shiny::uiOutput("widgets")
                   )

                 ),

                 # Main panel for displaying outputs ----
                 shiny::mainPanel(

                   shiny::p("Please select issue_type, status, resolved, component_s, custom_field_division_2,
                     custom_field_field, custom_field_start_date, and custom_field_workspace to view the
                     bar graphs"),
                   shiny::p("For the XML file, please ensure that start date is earlier than resolved date."),

                   # Output the XML plot
                   plotly::plotlyOutput("genPlot"),

                   # Output the 5 bar graphs
                   plotly::plotlyOutput("cePlot"),
                   plotly::plotlyOutput("niPlot"),
                   plotly::plotlyOutput("aiPlot"),
                   plotly::plotlyOutput("divPlot"),
                   plotly::plotlyOutput("wsPlot")

                   )
                 )
      ),

      # Debug Tab to check if tables are properly filtering
      shiny::tabPanel(
        "Debug", fluid = TRUE,

        # Output Data tables
        shiny::dataTableOutput("metrics"),
        shiny::dataTableOutput("ce"),
        shiny::dataTableOutput("ni"),
        shiny::dataTableOutput("sumTable"),
        shiny::dataTableOutput("ai"),
        shiny::dataTableOutput("div"),
        shiny::dataTableOutput("ws"),
        shiny::h3("Table for XML data"),
        shiny::dataTableOutput("xdf"),
        shiny::h3("Filtered xml data"),
        shiny::dataTableOutput("numIssues")
      )
    )
  )

  # Define server logic ---------------------------------------------------------------
  server <- function(input, output) {

    # Reactive values ----------------------------------------------------------
    getData <- shiny::reactive({

      shiny::validate(shiny::need(input$file, "No file is selected."))
      infile <- input$file

      if (tolower(tools::file_ext(infile$datapath)) == "csv") {

        output$fileUploaded <- shiny::renderText("csv")
        shiny::outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

        df <- janitor::clean_names(utils::read.csv(infile$datapath))

        return(df)

      } else {

        output$fileUploaded <- shiny::renderText("xml")
        shiny::outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

        df <- jiraR::xmltodf(infile$datapath)

        df <- janitor::clean_names(df)

        output$xdf <- shiny::renderDataTable(df)

        df <- df[!is.na(df$workspace), ]

        return(df)
      }
    })

    # Convert date columns in csv data frame from character type to Date type
    metrics <- shiny::reactive({

      shiny::req(getData())

      if (!is.null(getData()) & tools::file_ext(input$file$datapath) == "csv"){
        df <- dplyr::select(getData(), input$col)

        if ("custom_field_start_date" %in% colnames(df)){
          df[["custom_field_start_date"]] <- as.Date(df[["custom_field_start_date"]], format = '%Y-%m-%d')
        }

        if ("resolved" %in% colnames(df)){
          df[["resolved"]] <- as.Date(df[["resolved"]], format = '%Y-%m-%d')
        }

        if ("custom_field_due_date" %in% colnames(df)){
          df[["custom_field_due_date"]] <- as.Date(df[["custom_field_due_date"]], format = '%Y-%m-%d')
        }

        return(df)
      } else {
        return(NULL)
      }

    })

    # Number of closed epics per month for the last 12 months
    closedEpics <- shiny::reactive({

      shiny::req(metrics())

      df <- metrics() %>%

        # Filter for epics that are done in the last 12 months
        dplyr::filter(resolved %between% rev(seq(input$endDate, length.out = 2, by = paste(-1*input$p, "month"))),
               `issue_type` == "Epic",
               status == "Done") %>%

        # Group by month
        dplyr::group_by(format(resolved, "%m")) %>%

        # Count number of closed epics per month
        dplyr::summarise(`Closed Epics` = dplyr::n(), .groups = "drop")

      # Rename first column to Month
      names(df)[1] <- "Month"

      # Convert the integer character representation of Month to its full name
      df$Month <- month.name[as.integer(df$Month)]

      return(df)

    })

    # Number of new issues
    newIssues <- shiny::reactive({

      shiny::req(metrics())

      # Number of new issues each month for the last 12 months
      # Define new issues as Status = To Do (Can change this to be Status = To Do and In Progress)

      df <- metrics() %>%

        # Feature Addition: change the date values so that user can select the range instead of it being yearly
        # Filter for issues that are labeled To Do in the last 12 months
        dplyr::filter(`custom_field_start_date` %between% rev(seq(input$endDate2, length.out = 2,
                                                           by = paste(-1*input$p2, "month"))),
               status == "To Do",
               issue_type == "Epic") %>%

        # Group by month
        dplyr::group_by(format(`custom_field_start_date`, "%m")) %>%

        # Count number of new issues per month
        dplyr::summarise(`New Issues` = length(`custom_field_start_date`), .groups = "drop")

      # Rename first column to Month
      names(df)[1] <- "Month"

      # Convert the integer character representation of Month to its full name
      df$Month <- month.name[as.integer(df$Month)]

      return(df)
    })

    sumTable <- shiny::reactive({

      # Data frame of months
      months <- as.data.frame(c("January", "February", "March", "April", "May", "June", "July", "August",
                                "September", "October", "November", "December"))

      # Merge with Closed Epics and newIssues
      sumTable <- dplyr::full_join(months, closedEpics(), by = "Month")
      sumTable <- dplyr::full_join(sumTable, newIssues(), by = "Month")

      # Replace all NA values with 0
      sumTable[is.na(sumTable)] <- 0

      return(df)
    })

    ai <- shiny::reactive({

      shiny::req(metrics())

      # Data frame of months
      month <- as.data.frame(c("January", "February", "March", "April", "May", "June", "July", "August",
                               "September", "October", "November", "December"))

      names(month) <- "Month"

      df <- metrics() %>%

        dplyr::filter(`custom_field_start_date` %between% rev(seq(input$endDate3, length.out = 2,
                                                           by = paste(-1*input$p3, "month"))),
               status == "In Progress",
               issue_type == "Epic") %>%

        # Group by month
        dplyr::group_by(component_s, format(`custom_field_start_date`, "%m")) %>%

        dplyr::summarise(Count = length(component_s))

      # Rename the first column to Month
      names(df)[2] <- "Month"

      # Convert the integer character representation of Month to its full name
      df$Month <- month.name[as.integer(df$Month)]

      # Replace all "" values with None
      df[df == ""] <- "None"

      df <- tidyr::pivot_wider(df, names_from = component_s, values_from = Count)

      df <- dplyr::full_join(month, df, by = "Month")

      # Replace all NA values with 0
      df[is.na(df)] <- 0

      return(df)
      # Note: If a component had a count 0, it is not included. I.e. Surge Team Research had 0 issues
      # in the last 6 months; thus, there is no Surge Team Research column
    })

    div <- shiny::reactive({

      shiny::req(metrics())

      df <- metrics() %>%

        dplyr::filter(issue_type == "Epic") %>%

        # Group by Divison and Field
        dplyr::group_by(`custom_field_division_2`, `custom_field_field`) %>%

        # Count the number of issues
        dplyr::summarise(Count = dplyr::n(), .groups = "drop")

      # Replace all "" values with "No division"
      df[df == ""] <- "No division"

      # Pivot the data so it can be plotted
      df <- tidyr::pivot_wider(df, names_from = `custom_field_division_2`, values_from = Count)

      # Replace all NA values with 0
      df[is.na(df)] <- 0

      return(df)
    })

    ws <- shiny::reactive({

      shiny::req(metrics())

      df <- metrics() %>%

        dplyr::filter(issue_type == "Epic") %>%

        # Group by Workspace
        dplyr::group_by(`custom_field_workspace`) %>%

        # Count the number of issues
        dplyr::summarise(Count = dplyr::n()) #%>%

      # Replace empty value with NA (should clean data so there is no NA)
      df[df == ""] <- NA

      return(df)
    })

    # Count XML
    numIssues <- shiny::reactive({

      # Require all the filters
      shiny::req(input$issueType, input$status, input$jobType, input$assignee, input$reporter,
          input$component, input$div, input$field, input$ws, input$sd, input$period, input$rd,
          input$period2, input$groups)

      # Get the data
      df <- getData()

      # Filter
      df <- df %>%
        dplyr::filter(issue_type %in% input$issueType,
               status %in% input$status,
               job_type %in% input$jobType,
               assignee %in% input$assignee,
               reporter %in% input$reporter,
               component %in% input$component,
               division %in% input$div,
               field %in% input$field,
               workspace %in% input$ws,
               start_date %between% seq(input$sd, length.out = 2, by = paste(input$period, "month"))
        )

      # Only filter by resolved date when looking only for Done status
      if ("Done" %in% input$status & length(as.list(input$status)) == 1){
        df <- df %>% dplyr::filter(resolved_date %between% rev(seq(input$rd, length.out = 2,
                                                            by = paste(-1*input$period2, "month"))))
      }

      # Check if input$groups is not ""
      if (input$groups != "") {

        # Assign g to the list generated by groups
        g <- as.list(input$groups)

        # Group by
        # For each element in g
        for (j in 1:length(g)){

          # Check if Resolved or Start date is in the group
          if ("resolved_date" == g[[j]] | "start_date" == g[[j]]) {

            # Group by date column
            df <- dplyr::group_by(df, format(df[[g[[j]]]], "%m"), .add = TRUE)

            # Remove it from group list
            g[[j]] <- NULL
          }
        }

        # Check if group is empty
        if (length(g) != 0) {

          # Group by the remaining items in group
          df <- dplyr::group_by(df, .dots = g, .add = TRUE)
        }
      }

      # Count the number of issues after the filter(s) and group by
      df <- df %>% dplyr::summarise(Count = dplyr::n(), .groups = "drop")

      # Replace all "" in df with none (In case there are missing values)
      df[df == ""] <- "None"

      return(df)
    })

    pivot <- shiny::reactive({

      shiny::req(numIssues())

      # Get data frame with number of issues
      df <- numIssues()

      # Check if the data needs to be pivoted for plotting purposes
      if (input$pivot == TRUE & "component" %in% colnames(df) == FALSE) {
        df <- df %>% tidyr::pivot_wider(names_from = !!as.name(colnames(df)[1]), values_from = Count)
      } else if (input$pivot == TRUE & "component" %in% colnames(df) == TRUE) {

        # Specifically for Active Epics
        df <- df %>% tidyr::pivot_wider(names_from = !!as.name(colnames(df)[2]), values_from = Count)
      } else {
        # Replace all NA values with 0
        df[is.na(df)] <- 0
      }

      # Replace all NA values with 0
      df[is.na(df)] <- 0

      print(df)

      return(df)

    })

    # Output ----------------------------------------------------------------------------------------------

    output$col <- shiny::renderUI({
      shiny::selectizeInput(
        "col",
        shiny::h3("Select Metrics"),
        choices = colnames(getData()),
        multiple = TRUE,
        selected = c("issue_type", "status", "resolved", "component_s", "custom_field_division_2",
                     "custom_field_field", "custom_field_start_date", "custom_field_workspace")
      )
    })

    # List of buttons to create
    widgets <- shiny::reactive({

      df <- getData()

      list(
        shiny::selectizeInput("issueType",
                       label = "Issue Type",
                       choices = unique(df$issue_type),
                       multiple = TRUE,
                       selected = "Epic"),

        shiny::selectizeInput("status",
                       label = "Status",
                       choices = unique(df$status),
                       multiple = TRUE,
                       selected = unique(df$status)),

        shiny::selectizeInput("jobType",
                       label = "Job Type",
                       choices = unique(df$job_type),
                       multiple = TRUE,
                       selected = unique(df$job_type)),

        shiny::selectizeInput("assignee",
                       label = "Assignee",
                       choices = unique(df$assignee),
                       multiple = TRUE,
                       selected = unique(df$assignee)),

        shiny::selectizeInput("reporter",
                       label = "Reporter",
                       choices = unique(df$reporter),
                       multiple = TRUE,
                       selected = unique(df$reporter)),

        shiny::selectizeInput("component",
                       label = "Component",
                       choices = unique(df$component),
                       multiple = TRUE,
                       selected = unique(df$component)),

        shiny::selectizeInput("div",
                       label = "Division",
                       choices = unique(df$division),
                       multiple = TRUE,
                       selected = unique(df$division)),

        shiny::selectizeInput("field",
                       label = "Field",
                       choices = unique(df$field),
                       multiple = TRUE,
                       selected = unique(df$field)),

        shiny::selectizeInput("ws",
                       label = "Workspace",
                       choices = unique(df$workspace),
                       multiple = TRUE,
                       selected = unique(df$workspace)),

        shiny::dateInput("sd",
                  label = "Filter for when the issue started"),

        shiny::numericInput("period",
                     label = "Next X Months from Start Date",
                     value = 12,
                     min = 1
        ),

        shiny::dateInput("rd",
                  label = "Filter for when the issue ended"),

        shiny::numericInput("period2",
                     label = "Last X Months from Resolved Date",
                     value = 12,
                     min = 1
        ),

        shiny::selectizeInput(
          "groups",
          label = "Select columns to group by",
          choices = colnames(df),
          multiple = TRUE
        ),

        shiny::checkboxInput("pivot",
                      label = "Pivot Wider",
                      value = FALSE)

      )
    })

    # Output the filters and group by widgets
    output$widgets <- shiny::renderUI({widgets()})

    # Output bar graphs
    output$cePlot <- plotly::renderPlotly({

      shiny::req(sumTable())

      st <- sumTable()

      if (as.numeric(format(input$endDate, "%m")) < 12){
        recentMonths <- st[1:as.numeric(format(input$endDate, "%m")), ]
        oldMonths <- st[(as.numeric(format(input$endDate, "%m")) + 1):12, ]
        st <- rbind(oldMonths, recentMonths)
        st <- dplyr::slice(st, (13-input$p):dplyr::n())
      }

      cePlot <- plotly::plot_ly(st, x = ~Month, y = ~`Number of Closed Epics`, type = "bar") %>%
        plotly::layout(title = paste("Number of Closed Epics in the Last", input$p, "Months"),
               xaxis = list(title = "Month", categoryorder = "array", categoryarray = st$Month),
               yaxis = list(title = "Number of Closed Epics")
        )

      cePlot
    })

    output$niPlot <- plotly::renderPlotly({

      shiny::req(sumTable())

      st <- sumTable()

      if (as.numeric(format(input$endDate2, "%m")) < 12){
        recentMonths <- st[1:as.numeric(format(input$endDate2, "%m")), ]
        oldMonths <- st[(as.numeric(format(input$endDate2, "%m")) + 1):12, ]
        st <- rbind(oldMonths, recentMonths)
        st <- dplyr::slice(st, (13-input$p2):dplyr::n())
      }

      niPlot <- plotly::plot_ly(st, x = ~Month, y = ~`Number of New Issues`, type = "bar") %>%
        plotly::layout(title = paste("Number of New Epics in the Last", input$p2, "Months"),
               xaxis = list(title = "Month", categoryorder = "array", categoryarray = st$Month),
               yaxis = list(title = "Number of New Issues"))

      niPlot
    })

    output$aiPlot <- plotly::renderPlotly({

      shiny::req(ai())

      activeIssues <- ai()

      if (as.numeric(format(input$endDate3, "%m")) < 12){
        recentMonths <- activeIssues[1:as.numeric(format(input$endDate3, "%m")), ]
        oldMonths <- activeIssues[(as.numeric(format(input$endDate3, "%m")) + 1):12, ]
        activeIssues <- rbind(oldMonths, recentMonths)
        activeIssues <- dplyr::slice(activeIssues, (13-input$p3):dplyr::n())
      }

      # Create the base graph
      aiPlot <- plotly::plot_ly(data = activeIssues, x = ~Month)

      # Add each series one-by-one as new traces
      for (i in 2:length(colnames(activeIssues))) {
        aiPlot <- aiPlot %>%
          plotly::add_trace(x = activeIssues$Month, y = activeIssues[[i]],
                    type = "bar", name = colnames(activeIssues)[i])
      }

      aiPlot <- aiPlot %>%
        plotly::layout(title = paste("Number of Active Epics by Component in the Last", input$p3, "Months"),
               xaxis = list(title = "Month", categoryorder = "array", categoryarray = activeIssues$Month),
               yaxis = list(title = "Number of Active Issues"),
               annotations = list(x = 0.4, y = -0.1, text = "Note: Active Epics where the count for the Component is 0 are not shown",
                                  showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto',
                                  xshift=0, yshift=0, font=list(size=10, color="red"))
        )

      aiPlot
    })

    output$divPlot <- plotly::renderPlotly({

      shiny::req(div())

      divIssues <- div()

      # Colors (Total 433)
      colour = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

      # Create the base graph
      divPlot <- plotly::plot_ly(data = divIssues, x = ~`custom_field_field`)

      # Add each series one-by-one as new traces
      for (i in 2:length(colnames(divIssues))) {
        divPlot <- divPlot %>%
          plotly::add_trace(y = divIssues[[i]],
                    type = "bar",
                    name = colnames(divIssues)[i],
                    color = sample(colour, size = 1, replace = FALSE)
          )
      }

      divPlot <- divPlot %>%
        plotly::layout(title = "Number of Epics per Division by Field",
               xaxis = list(title = "Field"),
               yaxis = list(title = "Number of Epics"),
               barmode = "stack")

      divPlot
    })

    output$wsPlot <- plotly::renderPlotly({

      shiny::req(ws())

      wsPlot <- plotly::plot_ly(ws(), x = ~`custom_field_workspace`, y = ~Count, type = "bar") %>%
        plotly::layout(title = "Number of Epics by Workspace",
               xaxis = list(title = "Workspace"),
               yaxis = list(title = "Number of Epics"))

      wsPlot
    })

    output$genPlot <- plotly::renderPlotly({

      shiny::req(pivot())

      df <- pivot()

      if (names(df)[1] != "workspace") {
        names(df)[1] <- "Months"
        df$Months <- month.name[as.integer(df$Months)]
      }

      plotIssues(df)
    })

    # Output data tables
    output$metrics <- shiny::renderDataTable(metrics())
    output$ce <- shiny::renderDataTable(closedEpics())
    output$ni <- shiny::renderDataTable(newIssues())
    output$sumTable <- shiny::renderDataTable(sumTable())
    output$ai <- shiny::renderDataTable(ai())
    output$div <- shiny::renderDataTable(div())
    output$ws <- shiny::renderDataTable(ws())
    output$numIssues <- shiny::renderDataTable(pivot())
  }

  # Run the app -------------------------------------------------------
  shiny::shinyApp(ui = ui, server = server)
}
