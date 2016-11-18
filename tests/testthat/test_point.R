library(ncdf4)

# data prep.
# library(maptools)
# shapeData<-readShapePoints("data/se_sitest")
# i <- sapply(shapeData@data, is.factor)
# shapeData@data[i] <- lapply(shapeData@data[i], as.character)
# saveRDS(shapeData,file="data/se_points_data.rds")

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
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.7")
  expect_equivalent(ncatt_get(nc,varid="instance_name","standard_name")$value,"station_id")
  expect_equivalent(ncatt_get(nc,varid="instance_name","cf_role")$value,"timeseries_id")
  expect_equivalent(ncatt_get(nc,varid="lat","standard_name")$value,"latitude")
  expect_equivalent(ncatt_get(nc,varid="lon","standard_name")$value,"longitude")
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
  expect_equivalent(ncatt_get(nc,varid="instance_name","cf_role")$value,"timeseries_id")
  expect_equivalent(ncatt_get(nc,varid="lat","standard_name")$value,"latitude")
  expect_equivalent(ncatt_get(nc,varid="lon","standard_name")$value,"longitude")
})

test_that("multiPoint lat lon", {
  multipointData <- readRDS("data/multipointData.rds")
  lat<-multipointData@coords[,2]
  lon<-multipointData@coords[,1]
  alt<-1:length(lat)
  nc_file <- ToNCDFSG(nc_file=tempfile(), lons = lon, lats = lat, alts = alt)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1,2,3,4))
  expect_equal(as.numeric(ncvar_get(nc,'lat')),as.numeric(multipointData@coords[,2]))
  expect_equal(as.numeric(ncvar_get(nc,'lon')),as.numeric(multipointData@coords[,1]))
  expect_equal(as.numeric(ncvar_get(nc,'alt')),alt)
  expect_equivalent(ncatt_get(nc,varid="alt","standard_name")$value,"height")
})

test_that("shapefile_point", {
  pointData <- readRDS("data/se_points_data.rds")
  nc_file<-ToNCDFSG(nc_file = tempfile(), geomData = pointData, names = pointData@data$station_nm)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_true(all(names(pointData@data) %in% names(nc$var)))
  expect_equal(as.character(pointData@data$station_nm),as.character(ncvar_get(nc, nc$var$instance_name)))
  expect_equal(length(ncvar_get(nc, nc$var$lat)), length(pointData@coords[,2]))
  expect_equal(length(ncvar_get(nc, nc$var$lon)), length(pointData@coords[,1]))
  expect_equal(sum(ncvar_get(nc, nc$var$lat)), sum(pointData@coords[,2]))
  expect_equal(sum(ncvar_get(nc, nc$var$lon)), sum(pointData@coords[,1]))
})
