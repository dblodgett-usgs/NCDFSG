library(ncdf4)

context("NCDF SG point tests")

test_that("Point_timeSeries", {
  expect_error(ToNCDFSG("test"),regexp = "Did not find supported spatial data.")
  multipointData <- readRDS("data/pointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc,'lat')),as.numeric(multipointData@coords[,2]))
  expect_equal(as.numeric(ncvar_get(nc,'lon')),as.numeric(multipointData@coords[,1]))
  expect_equal(ncatt_get(nc,varid="lon","cf_role")$value,"geometry_x_node")
  expect_equal(ncatt_get(nc,varid="lat","cf_role")$value,"geometry_y_node")
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.7")
  expect_equivalent(ncatt_get(nc,varid="instance_name","standard_name")$value,"station_id")
  expect_equivalent(ncatt_get(nc,varid="lat","standard_name")$value,"latitude")
  expect_equivalent(ncatt_get(nc,varid="lon","standard_name")$value,"longitude")
  returnPointData<-FromNCDFSG(nc_file)
  expect_equal(as.numeric(multipointData@coords), as.numeric(returnPointData@coords))
  expect_equal(as.numeric(multipointData@bbox), as.numeric(returnPointData@bbox))
})

test_that("multiPoint_timeSeries", {
  multipointData <- readRDS("data/multipointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1,2,3,4))
  expect_equal(as.numeric(ncvar_get(nc,'lat')),as.numeric(multipointData@coords[,2]))
  expect_equal(as.numeric(ncvar_get(nc,'lon')),as.numeric(multipointData@coords[,1]))
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.7")
  expect_equivalent(ncatt_get(nc,varid="instance_name","standard_name")$value,"station_id")
  expect_equivalent(ncatt_get(nc,varid="lat","standard_name")$value,"latitude")
  expect_equivalent(ncatt_get(nc,varid="lon","standard_name")$value,"longitude")
  returnPointData<-FromNCDFSG(nc_file)
  expect_equal(as.numeric(multipointData@coords), as.numeric(returnPointData@coords))
  expect_equal(as.numeric(multipointData@bbox), as.numeric(returnPointData@bbox))
})

test_that("multiPoint lat lon alt", {
  multipointData <- readRDS("data/multipointData.rds")
  lat<-multipointData@coords[,2]
  lon<-multipointData@coords[,1]
  alt<-1:(length(lat))
  nc_file <- ToNCDFSG(nc_file=tempfile(), lons = lon, lats = lat, alts = alt)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1,2,3,4))
  expect_equal(as.numeric(ncvar_get(nc,'lat')),as.numeric(multipointData@coords[,2]))
  expect_equal(as.numeric(ncvar_get(nc,'lon')),as.numeric(multipointData@coords[,1]))
  expect_equal(as.numeric(ncvar_get(nc,'alt')),alt)
  expect_equivalent(ncatt_get(nc,varid="alt","standard_name")$value,"height")
  alt<-1:(length(lat)-2)
  expect_error(ToNCDFSG(nc_file=tempfile(), lons = lon, lats = lat, alts = alt),
               regexp = "station_names and alts must all be vectors of the same length")
  lat<-lat[1:(length(lat)-2)]
  expect_error(ToNCDFSG(nc_file=tempfile(), lons = lon, lats = lat),
               regexp = "station_names, lats, and lons must all be vectors of the same length")
  returnPointData<-FromNCDFSG(nc_file)
  expect_equal(as.numeric(multipointData@coords), as.numeric(returnPointData@coords))
  expect_equal(as.numeric(multipointData@bbox), as.numeric(returnPointData@bbox))
})
