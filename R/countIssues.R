#' Subset the data to find all missing values
#'
#' @param df data frame
#'
#' @return data frame that contains all the rows with NA
#' @export
missingData <- function(df){

  # Replace all empty strings with NA
  df[df == ""] <- NA

  # Subset the data frame to find all rows with NA
  missingData <- subset(df, is.na(df))

  # Return missing data
  missingData
}

#' Count the number of issues
#'
#' Allows the user to filter and group the JIRA metrics however they like, and then returns the
#' number of issues for their query
#'
#' @param df data frame
#' @param filt a list of column names to filter df with
#' @param filt_val a list of corresponding values being filtered for each column in f
#' @param group a list of column names
#' @param piv optional pivot of the data frame (a list pair: (Boolean, column name (string)))
#' @importFrom data.table %between%
#'
#' @return data frame with counts for the query
#' @export
countIssues <- function(df,
                        filt = list("Status"),
                        filt_val = list(list("To Do", "In Progress", "Done")),
                        group,
                        piv = list(FALSE, "")){

  # Assign df to countIssues
  countIssues <- df

  # Check that group is not empty
  if (length(group) == 0){

    # Return an error message
    return("Error: Group is empty")
  }

  # Check to see if filt and filt_val are the same length
  # If they aren't the same
  if (length(filt) != length(filt_val)){

    # Return an error message
    return("Error: The number of filters and the number of filter values do not match")

    # Else, they are the same length
  } else {

    # Filter countIssues for each item in filt and filt_val
    for (i in 1:length(filt)){

      # Check to see if the column in filt is in df
      if (filt[[i]] %in% names(df) == FALSE){

        # Return an error message
        return("Error: The column you tried to filter was not in the data frame")

        # Else, filter countIssues by filt and filt_val
      } else {

        # Check if filt_val's value is in the correct column
        # If it is false, it could be because of the vector pair for dates,
        # so check if the corresponding filter column is not Resolved or Start date.
        # Add to this list if other columns with dates are included
        if (filt_val[[i]] %in% countIssues[[filt[[i]]]] == FALSE &
            (filt[[i]] %in% c("Resolved", "Start date") == FALSE)){

          # Return an error message
          return("The value you were filtering did not exist in the column you filtering by")

          # Else, filt_val's values correspond to the right filter column
        } else {

          # Check if filt[i] is Resolved or Start date
          if (filt[[i]] %in% c("Resolved", "Start date")){

            # Check if filt_val for dates is in correct formet
            if (length(filt_val[[i]]) != 2 & !is.character(filt_val[[i]][[1]]) &
                !is.integer(filt_val[[i]][[2]]) &
                !is.na(filt_val[[i]][[1]]) & !is.na(filt_val[[i]][[2]])){

              # Return an error message
              return("Error: The date query was in the wrong format.
                     Please put it in the format c(\"yyyy-mm-dd\", integer)")

              # Filter countIssues by date
            } else {

              countIssues <- dplyr::filter(countIssues, !!as.name(filt[[i]]) %between%
                                      rev(seq(as.Date(filt_val[[i]][[1]]), length.out = 2,
                                              by = paste(-1*filt_val[[i]][[2]], "month"))
                                      )
              )
            }

            # Else, filter countIssues as normal
          } else {

            countIssues <- dplyr::filter(countIssues, !!as.name(filt[[i]]) %in% filt_val[[i]])

          }
        }
      }
    }
  }

  # Group by
  # For each element in group
  for (j in 1:length(group)){

    # Check if Resolved or Start date is in a vector
    if ("Resolved" %in% group[[j]] | "Start date" %in% group[[j]]) {

      # Check if the vector is length 2
      if (length(group[[j]]) != 2) {

        # Return an error message
        return("Error: The date group by query was wrong. Please put it in the format c(\"Column Name\", \"%unit of time\"). I.e. c(\"Resolved\", \"%m\")")

        # Else, group by date
      } else {

        # Group by date column
        countIssues <- dplyr::group_by(countIssues,
                                format(countIssues[[group[[j]][[1]]]], group[[j]][[2]]), .add = TRUE)

        # Remove it from group list
        group[[j]] <- NULL
      }
    }
  }

  # Check if group is empty
  if (length(group) != 0) {

    # Group by the remaining items in group
    countIssues <- dplyr::group_by(countIssues, .dots = group, .add = TRUE)
  }

  # Count the number of issues after the dplyr::filter(s) and group by
  countIssues <- countIssues %>%
    dplyr::summarise(Count = dplyr::n(), .groups = "drop")

  # Replace all "" in countIssues with none (In case there are missing values)
  countIssues[countIssues == ""] <- "None"

  # Check if the data needs to be pivoted for plotting purposes
  if (piv[[1]] == TRUE) {
    countIssues <- countIssues %>% tidyr::pivot_wider(names_from = !!as.name(piv[[2]]), values_from = Count)
  }

  # Replace all NA values with 0
  countIssues[is.na(countIssues)] <- 0

  # Return countIssues
  return(countIssues)
}

#' Count the number of closed epics per month for the last 12 months
#'
#' Create a data frame with the number of closed epics per month
#'
#' @param df data frame
#' @param rd resolved date; string - format YYYY-MM-DD
#' @param p period; int
#'
#' @return data frame with number of closed epics in the last year
#' @export
closedEpics <- function(df, rd = Sys.Date(), p = 12) {

  # Data frame of months
  months <- as.data.frame(c("January", "February", "March", "April", "May", "June", "July", "August",
                            "September", "October", "November", "December"))

  # Rename the first column to Month
  names(months) <- "Month"

  closedEpics <- df %>%

    # Filter for epics that are done in the last 12 months
    dplyr::filter(`Date Resolved` %between% rev(seq(rd, length.out = 2, by = paste(-1*p, "month"))),
           `Issue Type` == "Epic", Status == "Done" ) %>%

    # Group by month
    dplyr::group_by(format(`Date Resolved`, "%m")) %>%

    # Count number of closed epics per month
    dplyr::summarise(`Closed Epics` = dplyr::n(), .groups = "drop")

  # Rename first column to Month
  names(closedEpics)[1] <- "Month"

  # Convert the integer character representation of Month to its full name
  closedEpics$Month <- month.name[as.integer(closedEpics$Month)]

  # Merge
  closedEpics <- dplyr::full_join(months, closedEpics, by = "Month")

  # Replace NA with 0
  closedEpics[is.na(closedEpics)] <- 0

  # # Rolling months
  if (as.numeric(format(rd, "%m")) < 12){
    recentMonths <- closedEpics[1:as.numeric(format(rd, "%m")), ]
    oldMonths <- closedEpics[(as.numeric(format(rd, "%m")) + 1):12, ]
    closedEpics <- rbind(oldMonths, recentMonths)
    closedEpics <- dplyr::slice(closedEpics, (13-p):dplyr::n())
  }

  # Return closeEpics
  return(closedEpics)
}


#' Count the number of new issues per month for the last 12 months
#'
#' Create a data frame with the number of closed epics per month.
#' Define new issues as Status = To Do (Can change this to be Status = To Do and In Progress)
#'
#' @param df data frame
#' @param ds date start; string - format YYYY-MM-DD
#' @param p period; int
#' @importFrom data.table %between%
#'
#' @return data frame with number of new issues in the last year
#' @export
newIssues <- function(df, ds = Sys.Date(), p = 12){

  # Data frame of months
  months <- as.data.frame(c("January", "February", "March", "April", "May", "June", "July", "August",
                            "September", "October", "November", "December"))

  # Rename the first column to Month
  names(months) <- "Month"

  newIssues <- df %>%

    # Filter for issues that are labeled To Do in the last 12 months
    dplyr::filter(`Date Started` %between% rev(seq(ds, length.out = 2, by = paste(-1*p, "month"))),
           Status == "To Do",
           `Issue Type` == "Epic") %>%

    # Group by month
    dplyr::group_by(format(`Date Started`, "%m")) %>%

    # Count number of new issues per month
    dplyr::summarise(`New Issues` = dplyr::n(), .groups = "drop")

  # Rename first column to Month
  names(newIssues)[1] <- "Month"

  # Convert the integer character representation of Month to its full name
  newIssues$Month <- month.name[as.integer(newIssues$Month)]

  # Merge months
  newIssues <- dplyr::full_join(months, newIssues, by = "Month")

  # Replace NA with 0
  newIssues[is.na(newIssues)] <- 0

  # Rolling months
  if (as.numeric(format(ds, "%m")) < 12){
    recentMonths <- newIssues[1:as.numeric(format(ds, "%m")), ]
    oldMonths <- newIssues[(as.numeric(format(ds, "%m")) + 1):12, ]
    newIssues <- rbind(oldMonths, recentMonths)
    newIssues <- dplyr::slice(newIssues, (13-p):dplyr::n())
  }

  # Return newIssues
  return(newIssues)
}


#' A summary table of the issue counts in the last 12 months
#'
#' Create a data frame with the number of closed epics and new issues per month.
#'
#' @param df data frame
#'
#' @return data frame with summary of closed epics and new issues in the last year
#' @export
sumTable <- function(df){

  # Data frame of months
  months <- as.data.frame(c("January", "February", "March", "April", "May", "June", "July", "August",
                            "September", "October", "November", "December"))

  # Rename the first column to Month
  names(months) <- "Month"

  # Merge with Closed Epics and newIssues
  sumTable <- dplyr::full_join(months, closedEpics(df), by = "Month")
  sumTable <- dplyr::full_join(sumTable, newIssues(df), by = "Month")

  # Replace all NA values with 0
  sumTable[is.na(sumTable)] <- 0

  # Return sumTable
  return(sumTable)
}


#' Create a data frame with the distribution of active issues in the 5 components, by month
#' for the last 6 months
#'
#' @param df data frame
#' @param ds date start; string - format YYYY-MM-DD
#' @param p period; int
#' @importFrom data.table %between%
#'
#' @return data frame of active issues by component in the last 6 months
#' @export
activeIssues <- function(df, ds = Sys.Date(), p = 6){

  # Data frame of months
  month <- as.data.frame(c("January", "February", "March", "April", "May", "June", "July", "August",
                           "September", "October", "November", "December"))

  names(month) <- "Month"

  activeIssues <- df %>%

    dplyr::filter(`Date Started` %between% rev(seq(as.Date(ds), length.out = 2, by = paste(-1*p, "month"))),
           Status == "In Progress",
           `Issue Type` == "Epic") %>%

    dplyr::group_by(Component, format(`Date Started`, "%m")) %>%

    dplyr::summarise(Count = dplyr::n(), .groups = "drop")

  # Rename the second column to Month
  names(activeIssues)[2] <- "Month"

  # Convert the integer character representation of Month to its full name
  activeIssues$Month <- month.name[as.integer(activeIssues$Month)]

  # Replace all "" values with None
  activeIssues[activeIssues == ""] <- "None"

  # Pivot the components into columns
  activeIssues <- tidyr::pivot_wider(activeIssues, names_from = Component, values_from = Count)

  # Merge with Months
  activeIssues <- dplyr::full_join(month, activeIssues, by = "Month")

  # Replace all NA values with 0
  activeIssues[is.na(activeIssues)] <- 0

  # Rolling months
  if (as.numeric(format(ds, "%m")) < 12){
    recentMonths <- activeIssues[1:as.numeric(format(ds, "%m")), ]
    oldMonths <- activeIssues[(as.numeric(format(ds, "%m")) + 1):12, ]
    activeIssues <- rbind(oldMonths, recentMonths)
    activeIssues <- dplyr::slice(activeIssues, (13-p):dplyr::n())
  }

  activeIssues <- as.data.frame(activeIssues)

  # Return activeIssues
  return(activeIssues)
}


#' Create a data frame with the distribution of issues by division, grouped by field
#'
#' @param df data frame
#'
#' @return data frame of issues by division, grouped by field
#' @export
divIssues <- function(df){

  divIssues <- df %>%

    dplyr::filter(`Issue Type` == "Epic") %>%

    # Group by Divison and Field
    dplyr::group_by(Division, Field) %>%

    # Count the number of issues
    dplyr::summarise(Count = dplyr::n(), .groups = "drop")

  # Replace all "" values with "No division"
  divIssues[divIssues == ""] <- "No division"

  # Pivot the data so it can be plotted
  divIssues <- tidyr::pivot_wider(divIssues, names_from = Division, values_from = Count)

  # Replace all NA values with 0
  divIssues[is.na(divIssues)] <- 0

  # Return divIssues
  return(divIssues)
}


#' Create a data frame with the distribution of issues by workspaces
#'
#' @param df data frame
#'
#' @return data frame of the number of issues by workspace
#' @export
wsIssues <- function(df){

  wsIssues <- df %>%

    dplyr::filter(`Issue Type` == "Epic") %>%

    # Group by Workspace
    dplyr::group_by(Workspace) %>%

    # Count the number of issues
    dplyr::summarise(Count = dplyr::n(), .groups = "drop")

  # Replace all "" values with "None"
  wsIssues[wsIssues == ""] <- "None"

  # Return wsIssues
  return(wsIssues)
}
