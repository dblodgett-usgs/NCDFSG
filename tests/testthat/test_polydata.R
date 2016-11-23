library("ncdf4")

context("NCDF SG polygonData tests")

# data prep.
# library(maptools)
# shapeData<-readShapePoly("tests/testthat/data/Yahara_alb/Yahara_River_HRUs_alb_eq")
# saveRDS(shapeData,file="data/yahara_shapefile_data.rds")

test_that("A whole shapefile can be written", {
  yaharaData <- readRDS("data/yahara_shapefile_data.rds")
  nc_file <- ToNCDFSG(nc_file="test.nc", geomData = yaharaData)
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
  start <- nc$dim$coordinate_index$vals[stop_indices[(polyid-1)]] + 1
  stop <- nc$dim$coordinate_index$vals[(holeStarts[1]-1)]
  holeStart<-nc$dim$coordinate_index$vals[(holeStarts[1]+1)]
  holeStop <- nc$dim$coordinate_index$vals[stop_indices[polyid]]
  expect_equal(sum(as.numeric(yaharaData@polygons[[polyid]]@Polygons[[1]]@coords[,1])),
               sum(ncvar_get(nc, varid = "x", start = c(start), count = c((holeStarts[1]-start)))))
  expect_equal(sum(as.numeric(yaharaData@polygons[[polyid]]@Polygons[[2]]@coords[,1])),
               sum(ncvar_get(nc, varid = "x", start = holeStart, count = c((holeStop-holeStart)))))
  # i<-0
  # j<-0
  # for(g in 1:length(yaharaData@polygons)) {
  #   for(p in 1:length(yaharaData@polygons[[g]]@Polygons)) {
  #     if(p>1) {
  #       i<-i+1
  #       if(yaharaData@polygons[[g]]@Polygons[[p]]@hole) {
  #         expect_equal(nc$dim$coordinate_index$vals[i], -2)
  #       } else {expect_equal(nc$dim$coordinate_index$vals[i], -1) }
  #     }
  #     for(c in 1:length(yaharaData@polygons[[g]]@Polygons[[p]]@coords[,1])){
  #       i<-i+1
  #       j<-j+1
  #       expect_equal(nc$dim$coordinate_index$vals[i],j)
  #     }
  #   }
  #   expect_equal(nc$dim$coordinate_index$vals[stop_indices[g]],j)
  #   expect_equal(stop_indices[g], i)
  # }
  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(yaharaData, returnPolyData)
  # for(name in names(yaharaData@data)) {
  #   expect_equal(as.character(yaharaData@data[name]), as.character(returnPolyData@data[name]))
  # }
  # for(i in 1:length(returnPolyData@polygons)) {
  #   expect_equal(length(returnPolyData@polygons[[i]]@Polygons), length(yaharaData@polygons[[i]]@Polygons))
  #   for(j in 1:length(returnPolyData@polygons[[i]]@Polygons)) {
  #     expect_equal(length(returnPolyData@polygons[[i]]@Polygons[[j]]@coords), length(yaharaData@polygons[[i]]@Polygons[[j]]@coords))
  #   }
  # }
})

