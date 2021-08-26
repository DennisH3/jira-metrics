#' JIRA R Package
#'
#' Create a package that summarizes and plots the following metrics:
#'  - Number of closed epics per month for the last 12 months.
#'  - Number of new issues each month for the last 12 months.
#'  - Distribution of active issues in the 5 components, by month for the last 6 months.
#'  - Distribution of issues by division, grouped by field (likely a stacked bar chart).
#'  - Distribution of issues by workspaces.
#'
#' @docType package
#' @name jiraR-package
#' @aliases jira-metrics
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom data.table %between%
#'
utils::globalVariables(c("Count", "Date Started", "Date Resolved", "Division", "Field", "Issue Type",
                         "Component", "Status", "Workspace", "assignee", "component", "component_s",
                         "custom_field_division_2", "custom_field_field", "custom_field_start_date",
                         "custom_field_workspace", "division", "field", "issue_type", "job_type",
                         "reporter", "resolved", "resolved_date", "start_date", "status", "workspace"))
