library(ncdf4)

context("NCDF SG line tests")

test_that("linedata works", {
  lineData <- readRDS("data/lineData.rds")
  nc_file <- geom_timeSeries(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc, "x")), as.numeric(lineData@lines[[1]]@Lines[[1]]@coords[,1]))
  expect_equal(as.numeric(ncvar_get(nc, "y")), as.numeric(lineData@lines[[1]]@Lines[[1]]@coords[,2]))
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.8")
  expect_equivalent(ncatt_get(nc,varid=0,"cdm_data_type")$value,"line")
  expect_equivalent(ncatt_get(nc,varid=0,"standard_name_vocabulary")$value,"CF-1.8")
  expect_equivalent(ncatt_get(nc,varid="instance_name","standard_name")$value,"instance_id")
  expect_equivalent(ncatt_get(nc,varid="instance_name","cf_role")$value,"timeseries_id")
  expect_equivalent(ncatt_get(nc,varid="x","standard_name")$value,"geometry x node")
  expect_equivalent(ncatt_get(nc,varid="y","standard_name")$value,"geometry y node")
})

test_that("multiLine data works", {
  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- geom_timeSeries(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)
})
