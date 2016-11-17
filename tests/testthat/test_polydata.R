library("ncdf4")

context("NCDF SG polydata shape tests")

# data prep.
# library(maptools)
# shapeData<-readShapePoly("tests/testthat/data/Yahara_alb/Yahara_River_HRUs_alb_eq")
# saveRDS(shapeData,file="data/yahara_shapefile_data.rds")

test_that("A whole shapefile can be written", {
  yaharaData <- readRDS("data/yahara_shapefile_data.rds")
  nc_file <- geom_timeSeries(nc_file=tempfile(), geomData = yaharaData)
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
})
