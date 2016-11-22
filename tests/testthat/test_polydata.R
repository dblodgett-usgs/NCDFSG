library("ncdf4")

compareSP <- function(polygonData, returnPolyData) {
  expect_equal(length(polygonData@polygons[[1]]@Polygons), length(returnPolyData@polygons[[1]]@Polygons))
  for(i in 1:length(length(polygonData@polygons[[1]]@Polygons))) {
    expect_equal(as.numeric(returnPolyData@polygons[[1]]@Polygons[[i]]@coords),
                 as.numeric(polygonData@polygons[[1]]@Polygons[[i]]@coords))
    # expect_equal(polygonData@polygons[[1]]@Polygons[[i]], returnPolyData@polygons[[1]]@Polygons[[i]]) # checks attributes, not sure it's work testing them.
  }
  expect_equal(polygonData@polygons[[1]]@area, returnPolyData@polygons[[1]]@area)
  # expect_equal(polygonData@polygons[[1]]@plotOrder, returnPolyData@polygons[[1]]@plotOrder) # Don't want to worry about plot order right now.
  expect_equal(polygonData@polygons[[1]]@labpt, returnPolyData@polygons[[1]]@labpt)
  # expect_equal(polygonData@polygons[[1]]@ID, returnPolyData@polygons[[1]]@ID) # maptools 0 indexes others 1 index. Not roundtripping this yet.
}

context("NCDF SG polydata shape tests")

# data prep.
# library(maptools)
# shapeData<-readShapePoly("tests/testthat/data/Yahara_alb/Yahara_River_HRUs_alb_eq")
# saveRDS(shapeData,file="data/yahara_shapefile_data.rds")

test_that("A whole shapefile can be written", {
  yaharaData <- readRDS("data/yahara_shapefile_data.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = yaharaData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(as.numeric(yaharaData@data$GRIDCODE),as.numeric(ncvar_get(nc, varid = "GRIDCODE")))
  expect_equal(length(nc$dim$instance$vals), length(yaharaData@polygons))
  expect_equal(as.numeric(yaharaData@polygons[[1]]@Polygons[[1]]@coords[,1]),as.numeric(ncvar_get(nc, varid = "x", start = c(1), count = c(118))))
  expect_equal(as.numeric(yaharaData@polygons[[1]]@Polygons[[1]]@coords[,2]),as.numeric(ncvar_get(nc, varid = "y", start = c(1), count = c(118))))
  # Check to make sure a hole is encoded correctly.
  stop_indices <- ncvar_get(nc, "coordinate_index_stop")
  holeStarts<-which(nc$dim$coordinate_index$vals == -2)

  difs<-(stop_indices-holeStarts[1]); difs[difs < 0] <- NA
  polyid <- which(difs==min(difs,na.rm = TRUE))
  start <- stop_indices[(polyid-1)] + 1
  stop <- nc$dim$coordinate_index$vals[(holeStarts[1]-1)]
  holeStart<-nc$dim$coordinate_index$vals[(holeStarts[1]+1)]
  holeStop <- stop_indices[polyid]
  expect_equal(sum(as.numeric(yaharaData@polygons[[polyid]]@Polygons[[1]]@coords[,1])),
               sum(ncvar_get(nc, varid = "x", start = c(start), count = c((holeStarts[1]-start)))))
  expect_equal(sum(as.numeric(yaharaData@polygons[[polyid]]@Polygons[[2]]@coords[,1])),
               sum(ncvar_get(nc, varid = "x", start = holeStart, count = c((holeStop-holeStart)))))
  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(yaharaData, returnPolyData)
  for(name in names(yaharaData@data)) {
    expect_equal(as.character(yaharaData@data[name]), as.character(returnPolyData@data[name]))
  }
})
