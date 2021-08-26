test_that("sumTable runs without error", {

  testData <- read.csv(test_path("testData.csv"), check.names = FALSE)

  testData[["Date Started"]] <- as.Date(testData[["Date Started"]])
  testData[["Date Resolved"]] <- as.Date(testData[["Date Resolved"]])

  expect_error(sumTable(testData), NA)
})
