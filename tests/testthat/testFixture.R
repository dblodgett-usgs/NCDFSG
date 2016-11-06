library(ncdf4)

context("NCDF SG Base Fixture Tests")

# Data Prep
# library(jsonlite)
# library(rgeos)
# fixtureData<-fromJSON(readLines(system.file('extdata','fixture_wkt.json', package = 'NCDFSG'),warn = FALSE))
# multipointData <- readWKT(fixtureData[["2d"]]$multipoint)
# saveRDS(multipointData,file="tests/testthat/data/multiPointData.rds")

test_that("multiPoint_timeSeries runs", {
  multipointData <- readRDS("data/multipointData.rds")
  nc_file <- multiPoint_timeSeries(nc_file=tempfile(), multiPoint = multipointData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$station$vals,c(1,2,3,4))
  expect_equal(as.numeric(ncvar_get(nc,'lat')),as.numeric(multipointData@coords[,2]))
  expect_equal(as.numeric(ncvar_get(nc,'lon')),as.numeric(multipointData@coords[,1]))
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.7")
  expect_equivalent(ncatt_get(nc,varid=0,"cdm_data_type")$value,"Station")
  expect_equivalent(ncatt_get(nc,varid=0,"standard_name_vocabulary")$value,"CF-1.7")
  expect_equivalent(ncatt_get(nc,varid="station_name","standard_name")$value,"station_id")
  expect_equivalent(ncatt_get(nc,varid="station_name","cf_role")$value,"timeseries_id")
  expect_equivalent(ncatt_get(nc,varid="lat","standard_name")$value,"latitude")
  expect_equivalent(ncatt_get(nc,varid="lon","standard_name")$value,"longitude")
})
