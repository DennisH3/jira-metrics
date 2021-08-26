#' Plot any filtered data frame
#'
#' @param df data frame
#'
#' @return plotly bar graph
#' @export
plotIssues <- function(df){

  plotDF <- df

  # Colors (Total 433)
  colour = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

  # Create the base graph
  fig <- plotly::plot_ly(data = plotDF, x = ~plotDF[[1]])

  # Add each series one-by-one as new traces
  for (i in 2:length(colnames(plotDF))) {
    fig <- fig %>%
      plotly::add_trace(y = plotDF[[i]],
                type = "bar",
                name = colnames(plotDF)[i],
                color = sample(colour, size = 1, replace = FALSE)
      )
  }

  # Configure layout
  fig <- fig %>%
    plotly::layout(title = paste("Number of Epics by", colnames(plotDF)[[1]]),
           xaxis = list(title = colnames(plotDF)[[1]]),
           yaxis = list(title = "Number of Epics"),
           barmode = "stack")

  # Return fig
  return(fig)
}


#' Plot the number of closed epics per month for the last 12 months
#'
#' @param df data frame
#'
#' @return plotly bar graph
#' @export
cePlot <- function(df){

  st <- closedEpics(df)

  cePlot <- plotly::plot_ly(st, x = ~Month, y = ~`Closed Epics`, type = "bar") %>%
    plotly::layout(title = "Number of Closed Epics in the Last 12 Months",
           xaxis = list(title = "Month", categoryorder = "array", categoryarray = st$Month),
           yaxis = list(title = "Number of Closed Epics"))

  return(cePlot)
}


#' Plot the number of new epics per month for the last 12 months
#'
#' @param df data frame
#'
#' @return plotly bar graph
#' @export
niPlot <- function(df){

  st <- newIssues(df)

  niPlot <- plotly::plot_ly(st, x = ~Month, y = ~`New Issues`, type = "bar") %>%
    plotly::layout(title = "Number of New Epics in the Last 12 Months",
           xaxis = list(title = "Month", categoryorder = "array", categoryarray = st$Month),
           yaxis = list(title = "Number of New Epics"))

  return(niPlot)
}


#' Plot active epics by components by month for the last 6 months
#'
#' @param df data frame
#'
#' @return plotly bar graph
#' @export
aiPlot <- function(df){

  activeIssues <- activeIssues(df)

  # Create the base graph
  aiPlot <- plotly::plot_ly(data = activeIssues, x = ~Month)

  # Add each series one-by-one as new traces
  for (i in 2:length(colnames(activeIssues))) {
    aiPlot <- aiPlot %>%
      plotly::add_trace(x = activeIssues$Month, y = activeIssues[[i]],
                type = "bar", name = colnames(activeIssues)[i])
  }

  aiPlot <- aiPlot %>%
    plotly::layout(title = "Number of Active Epics by Component in the Last 6 Months",
           xaxis = list(title = "Month", categoryorder = "array", categoryarray = activeIssues$Month),
           yaxis = list(title = "Number of Active Epics"),
           annotations = list(x = 0.4, y = -0.1, text = "Note: Active Epics where the count for the Component is 0 are not shown",
                              showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto',
                              xshift=0, yshift=0, font=list(size=10, color="red"))
    )

  aiPlot
}


#' Plot distribution of epics by division, grouped by field
#'
#' @param df data frame
#'
#' @return plotly bar graph
#' @export
divPlot <- function(df){

  # Colors (Total 433)
  colour = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

  div <- jiraR::divIssues(df)

  # Create the base graph
  divPlot <- plotly::plot_ly(data = div, x = ~Field)

  # Add each series one-by-one as new traces
  for (i in 2:length(colnames(div))) {
    divPlot <- divPlot %>%
      plotly::add_trace(y = div[[i]],
                type = "bar",
                name = colnames(div)[i],
                color = sample(colour, size = 1, replace = FALSE)
      )
  }

  divPlot <- divPlot %>%
    plotly::layout(title = "Number of Epics per Division by Field",
           xaxis = list(title = "Field"),
           yaxis = list(title = "Number of Epics"),
           barmode = "stack")

  divPlot
}


#' Plot distribution of epics by workspaces
#'
#' @param df data frame
#'
#' @return plotly bar graph
#' @export
wsPlot <- function(df){
  wsPlot <- plotly::plot_ly(wsIssues(df), x = ~Workspace, y = ~Count, type = "bar") %>%
    plotly::layout(title = "Number of Epics by Workspace",
           xaxis = list(title = "Workspace"),
           yaxis = list(title = "Number of Epics"))

  wsPlot
}
