#' Convert an XML file to a Data Frame
#'
#' @param xml_file data frame
#'
#' @return data frame
#' @export
xmltodf <- function(xml_file){

  # Read the document
  doc <- xml2::read_xml(xml_file)

  # Extract all the elements and create the data frame
  df <- xml2::xml_find_all(doc, ".//item") %>%
    purrr::map_df(function(x) {
      list(

        # Get Issue Key
        key = xml2::xml_find_first(x, ".//key") %>%
          xml2::xml_text(),

        # Get Summary
        issueName = xml2::xml_find_first(x, ".//summary") %>%
          xml2::xml_text(),

        # Get Issue Type
        issueType = xml2::xml_find_first(x, ".//type") %>%
          xml2::xml_text(),

        # Get Status
        status = xml2::xml_find_first(x, ".//status") %>%
          xml2::xml_text(),

        # Get Label
        label = xml2::xml_find_first(x, ".//labels/label") %>%
          xml2::xml_text(),

        # Get Assignee
        assignee = xml2::xml_find_first(x, ".//assignee") %>%
          xml2::xml_text(),

        # Get Reporter
        reporter = xml2::xml_find_first(x, ".//reporter") %>%
          xml2::xml_text(),

        # Get Component/s
        component = xml2::xml_find_first(x, ".//component") %>%
          xml2::xml_text(),

        # Get Division
        division = xml2::xml_find_first(x, ".//customfield[@id='customfield_16622']") %>%
          xml2::xml_text() %>%
          stringr::str_remove("Division"),

        # Get Field
        field = xml2::xml_find_first(x, ".//customfield[@id='customfield_14028']") %>%
          xml2::xml_text() %>%
          stringr::str_remove("Field"),

        # Get Workspace
        ws = xml2::xml_find_first(x, ".//customfield[@id='customfield_16203']") %>%
          xml2::xml_text() %>%
          stringr::str_remove("Workspace"),

        # Get Start date and format it to be easily converted to Date type
        sd = xml2::xml_find_first(x, ".//customfield[@id='customfield_13907']") %>%
          xml2::xml_text() %>%
          stringr::str_remove("Start date") %>%
          stringr::str_sub(6, -16) %>%
          stringr::str_replace_all(" ", "") %>%
          tolower(),

        # Get Resolved date and format it to be easily converted to Date type
        resolvedDate = xml2::xml_find_first(x, ".//resolved") %>%
          xml2::xml_text() %>%
          stringr::str_sub(6, -16) %>%
          stringr::str_replace_all(" ", "") %>%
          tolower(),

        # Get Due date and format it to be easily converted to Date type
        dueDate = xml2::xml_find_first(x, ".//customfield[@id='customfield_14066']") %>%
          xml2::xml_text() %>%
          stringr::str_sub(6, -16) %>%
          stringr::str_replace_all(" ", "") %>%
          tolower()
      )
    })

  # Convert sd, resolved, and dueDate to date types
  df[["sd"]] <- as.Date(df[["sd"]], format = "%d%B%Y")
  df[["resolvedDate"]] <- as.Date(df[["resolvedDate"]], format = "%d%B%Y")
  df[["dueDate"]] <- as.Date(df[["dueDate"]], format = "%d%B%Y")

  # Rename columns
  names(df) <- c("Issue Key", "Summary", "Issue Type", "Status", "Job Type", "Assignee", "Reporter",
                 "Component", "Division", "Field", "Workspace", "Start Date", "Resolved Date", "Due Date")

  # Return df
  return(df)
}
