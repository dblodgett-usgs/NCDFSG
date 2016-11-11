library(ncdf4)

context("NCDF SG line tests")

test_that("linedata works", {
  lineData <- readRDS("data/lineData.rds")
  nc_file <- line_timeSeries(nc_file=tempfile(), line = lineData)
  nc<-nc_open(nc_file)
})

test_that("multiLine data works", {
  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- line_timeSeries(nc_file=tempfile(), line = lineData)
  nc<-nc_open(nc_file)
})
