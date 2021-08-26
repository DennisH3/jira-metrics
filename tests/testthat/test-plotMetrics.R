test_that("Graphing functions return plotly objects", {

  # Note: date columns were read as character type
  testData <- read.csv(test_path("testData.csv"), check.names = FALSE)

  # Convert to date type
  testData[["Date Started"]] <- as.Date(testData[["Date Started"]])
  testData[["Date Resolved"]] <- as.Date(testData[["Date Resolved"]])

  expect_is(cePlot(testData), "plotly")
  expect_is(niPlot(testData), "plotly")
  expect_is(aiPlot(testData), "plotly")
  expect_is(divPlot(testData), "plotly")
  expect_is(wsPlot(testData), "plotly")
})
