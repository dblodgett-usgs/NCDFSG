library("ncdf4")

context("NCDF SG polygonData tests")

# data prep.
# library(maptools)
# shapeData<-readShapePoly("tests/testthat/data/Yahara_alb/Yahara_River_HRUs_alb_eq")
# saveRDS(shapeData,file="data/yahara_shapefile_data.rds")

test_that("A whole shapefile can be written", {
  polygonData <- readRDS("data/yahara_shapefile_data.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)
  expect_equal(as.numeric(polygonData@data$GRIDCODE),as.numeric(ncvar_get(nc, varid = "GRIDCODE")))
  expect_equal(length(nc$dim$instance$vals), length(polygonData@polygons))
  expect_equal(as.numeric(polygonData@polygons[[1]]@Polygons[[1]]@coords[,1]),as.numeric(ncvar_get(nc, varid = "x", start = c(1), count = c(118))))
  expect_equal(as.numeric(polygonData@polygons[[1]]@Polygons[[1]]@coords[,2]),as.numeric(ncvar_get(nc, varid = "y", start = c(1), count = c(118))))
  # Check to make sure a hole is encoded correctly.
  node_count <- ncvar_get(nc, "node_count")
  part_node_count <- ncvar_get(nc, "part_node_count")
  part_type <- ncvar_get(nc, "part_type")
  expect_equal(length(polygonData@polygons), length(node_count))
  p <- 1
  for(i in 1:length(node_count)) {
    nCount <- 0
    for(j in 1:length(polygonData@polygons[[i]]@Polygons)) {
      if(polygonData@polygons[[i]]@Polygons[[j]]@hole) expect_equal(part_type[p], -2)
      expect_equal(length(polygonData@polygons[[i]]@Polygons[[j]]@coords[,1]), part_node_count[p])
      nCount <- nCount + part_node_count[p]
      p <- p + 1
    }
    expect_equal(nCount, node_count[i])
  }

  # checkAllPoly(nc, polygonData, nc$dim$coordinate_index$vals, ncvar_get(nc, nc$var$coordinate_index_stop))
  # returnPolyData<-FromNCDFSG(nc_file)
  # compareSP(polygonData, returnPolyData)
  # for(name in names(polygonData@data)) {
  #   expect_equal(as.character(polygonData@data[name]), as.character(returnPolyData@data[name]))
  # }
  # for(i in 1:length(returnPolyData@polygons)) {
  #   expect_equal(length(returnPolyData@polygons[[i]]@Polygons), length(polygonData@polygons[[i]]@Polygons))
  #   for(j in 1:length(returnPolyData@polygons[[i]]@Polygons)) {
  #     expect_equal(length(returnPolyData@polygons[[i]]@Polygons[[j]]@coords), length(polygonData@polygons[[i]]@Polygons[[j]]@coords))
  #   }
  # }
  # writePolyShape(returnPolyData, "yaharaData_test")
})

