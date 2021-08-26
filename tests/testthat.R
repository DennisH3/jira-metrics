# Source: https://stackoverflow.com/questions/41846465/testthat-fails-within-devtoolscheck-but-works-in-devtoolstest
Sys.setenv(R_TESTS="")

library(testthat)
library(jiraR)

test_check("jiraR")
