library(ncdf4)

context("NCDF SG Base Fixture Tests")

# Data Prep - this could be included in tests, but then rgeos would be required for tests.
# library(jsonlite)
# library(rgeos)
# fixtureData<-fromJSON(readLines(system.file('extdata','fixture_wkt.json', package = 'NCDFSG'),warn = FALSE))
# multipointData <- readWKT(fixtureData[["2d"]]$multipoint)
# pointData <- readWKT(fixtureData[["2d"]]$point)
# lineData <- readWKT(fixtureData[["2d"]]$linestring)
# multiLineData <- readWKT(fixtureData[["2d"]]$multilinestring)
# polygonData <- readWKT(fixtureData[["2d"]]$polygon)
# polygon_holeData <- readWKT(fixtureData[["2d"]]$polygon_hole)
# multipolygonData <- readWKT(fixtureData[["2d"]]$multipolygon)
# multipolygon_holeData <- readWKT(fixtureData[["2d"]]$multipolygon_hole)
# multipolygons_holesData <- readWKT(fixtureData[["2d"]]$multipolygons_holes)
# multigeometries_polygons_holesData <- readWKT(paste0("GEOMETRYCOLLECTION(",fixtureData[["2d"]]$multipolygons_holes,", ",fixtureData[["2d"]]$multipolygon_hole,")"), id = c("1","2"))
# saveRDS(multipointData,file="data/multiPointData.rds")
# saveRDS(pointData,file="data/pointData.rds")
# saveRDS(lineData, file="data/lineData.rds")
# saveRDS(multiLineData, file="data/multiLineData.rds")
# saveRDS(polygonData, file="data/polygonData.rds")
# saveRDS(polygon_holeData, file="data/polygon_holeData.rds")
# saveRDS(multipolygonData, file="data/multipolygonData.rds")
# saveRDS(multipolygon_holeData, file="data/multipolygon_holeData.rds")
# saveRDS(multipolygons_holesData, file="data/multipolygons_holes.rds")
# saveRDS(multigeometries_polygons_holesData, file="data/multigeometries_polygons_holes.rds")

test_that("multiPoint_timeSeries", {
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

test_that("polygon_timeSeries for basic polygon", {
  polygonData <- readRDS("data/polygonData.rds")
  nc_file <- polygon_timeSeries(nc_file=tempfile(), polygon = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc,'x')),
               as.numeric(polygonData@polygons[[1]]@Polygons[[1]]@coords[,1]))
  expect_equal(as.numeric(ncvar_get(nc,'y')),
               as.numeric(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]))
  expect_equal(as.numeric(ncvar_get(nc,'coordinate_index_stop')),
               length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]))
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.7")
  expect_equivalent(ncatt_get(nc,varid=0,"cdm_data_type")$value,"polygon")
  expect_equivalent(ncatt_get(nc,varid=0,"standard_name_vocabulary")$value,"CF-1.7")
  expect_equivalent(ncatt_get(nc,varid="instance_name","standard_name")$value,"instance_id")
  expect_equivalent(ncatt_get(nc,varid="instance_name","cf_role")$value,"timeseries_id")
})

test_that("polygon_timeSeries for polygon with a hole.", {
  polygonData <- readRDS("data/polygon_holeData.rds")
  nc_file <- polygon_timeSeries(nc_file=tempfile(), polygon = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc,'coordinate_index_stop')),
               (length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]) +
                 length(polygonData@polygons[[1]]@Polygons[[2]]@coords[,2])))
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[6],-2) # manually verified this is right.
  expect_equal(length(nc$dim$coordinate_index$vals), 10)
})

test_that("polygon_timeSeries for multipolygon.", {
  polygonData <- readRDS("data/multipolygonData.rds")
  nc_file <- polygon_timeSeries(nc_file=tempfile(), polygon = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc,'coordinate_index_stop')),
               (length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]) +
                  length(polygonData@polygons[[1]]@Polygons[[2]]@coords[,2])))
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[5],-1)
  expect_equal(length(nc$dim$coordinate_index$vals), 10)
})

test_that("polygon_timeSeries for a multipolygon with a hole.", {
  polygonData <- readRDS("data/multipolygon_holeData.rds")
  nc_file <- polygon_timeSeries(nc_file=tempfile(), polygon = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc,'coordinate_index_stop')),
               (length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]) +
                length(polygonData@polygons[[1]]@Polygons[[2]]@coords[,2]) +
                length(polygonData@polygons[[1]]@Polygons[[3]]@coords[,2])))
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[7],-1)
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[12],-2)
  expect_equal(length(nc$dim$coordinate_index$vals), (as.numeric(ncvar_get(nc,'coordinate_index_stop'))+2))
})

test_that("polygon_timeSeries for multipolygons with holes.", {
  polygonData <- readRDS("data/multipolygons_holes.rds")
  nc_file <- polygon_timeSeries(nc_file=tempfile(), polygon = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[16],-1)
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[6],-2)
  expect_equal(length(nc$dim$coordinate_index$vals), (as.numeric(ncvar_get(nc,'coordinate_index_stop'))+5))
})
