library(ncdf4)

context("NCDF SG polygon tests")

test_that("polygon_timeSeries for basic polygon", {
  polygonData <- readRDS("data/polygonData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc,'x')),
               as.numeric(polygonData@polygons[[1]]@Polygons[[1]]@coords[,1]))
  expect_equal(as.numeric(ncvar_get(nc,'y')),
               as.numeric(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]))
  expect_equal(as.numeric(ncvar_get(nc,'coordinate_index_stop')),
               length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]))
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.8")
  expect_equivalent(ncatt_get(nc,varid="instance_name","standard_name")$value,"instance_id")
  expect_equivalent(ncatt_get(nc,varid="x","standard_name")$value,"geometry x node")
  expect_equivalent(ncatt_get(nc,varid="y","standard_name")$value,"geometry y node")
  expect_equivalent(ncatt_get(nc,varid="coordinate_index","geom_coordinates")$value,"x y")
  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("polygon_timeSeries for polygon with a hole.", {
  polygonData <- readRDS("data/polygon_holeData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(nc$dim$coordinate_index$vals[ncvar_get(nc,'coordinate_index_stop')]),
               (length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]) +
                 length(polygonData@polygons[[1]]@Polygons[[2]]@coords[,2])))
  expect_equal(as.numeric(ncvar_get(nc,'coordinate_index_stop')),
               (length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]) +
                  length(polygonData@polygons[[1]]@Polygons[[2]]@coords[,2]))+1)
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[6],-2) # manually verified this is right.
  expect_equal(length(nc$dim$coordinate_index$vals), 10)
  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("polygon_timeSeries for multipolygon.", {
  polygonData <- readRDS("data/multipolygonData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc,'coordinate_index_stop')),
               (length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]) +
                  length(polygonData@polygons[[1]]@Polygons[[2]]@coords[,2]))+1) # +1 for the extracoord.
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[5],-1)
  expect_equal(length(nc$dim$coordinate_index$vals), 10)
  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("polygon_timeSeries for a multipolygon with a hole.", {
  polygonData <- readRDS("data/multipolygon_holeData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc,'coordinate_index_stop')),
               (length(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]) +
                length(polygonData@polygons[[1]]@Polygons[[2]]@coords[,2]) +
                length(polygonData@polygons[[1]]@Polygons[[3]]@coords[,2]))+2) # +2 for two extracoords.
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[5],-1)
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[12],-2)
  expect_equal(length(nc$dim$coordinate_index$vals), as.numeric(ncvar_get(nc,'coordinate_index_stop')))
  expect_equal(length(ncvar_get(nc,"x")), (as.numeric(ncvar_get(nc,'coordinate_index_stop'))-2)) # Two for extracoords.
  expect_equal(length(ncvar_get(nc, "x")), nc$dim$coordinate_index$vals[ncvar_get(nc,'coordinate_index_stop')]) # Check indexes are clean.
  checkAllPoly(nc, polygonData, nc$dim$coordinate_index$vals, ncvar_get(nc, nc$var$coordinate_index_stop))
  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("polygon_timeSeries for multipolygons with holes.", {
  polygonData <- readRDS("data/multipolygons_holes.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[21],-1)
  expect_equal(as.numeric(nc$dim$coordinate_index$vals)[6],-2)
  expect_equal(length(nc$dim$coordinate_index$vals), as.numeric(ncvar_get(nc,'coordinate_index_stop')))
  expect_equal(length(ncvar_get(nc,"x")), (as.numeric(ncvar_get(nc,'coordinate_index_stop'))-5)) # Five for extracoords.
  expect_equal(length(ncvar_get(nc, "x")), nc$dim$coordinate_index$vals[ncvar_get(nc,'coordinate_index_stop')]) # Check indexes are clean.
  checkAllPoly(nc, polygonData, nc$dim$coordinate_index$vals, ncvar_get(nc, nc$var$coordinate_index_stop))
  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})
