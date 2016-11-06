library(jsonlite)
context("NCDF DSG Base Fixture Tests")

test_that("json data loads ok", {
  fixtureData<-fromJSON(readLines(system.file(
    'extdata','fixture_wkt.json', package = 'NCDFSG'),warn = FALSE))
  expect_equal(length(fixtureData),2)
})

