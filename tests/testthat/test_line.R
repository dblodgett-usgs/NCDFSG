library(ncdf4)

context("NCDF SG line tests")

test_that("linedata works", {
  lineData <- readRDS("data/lineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc, "x")), as.numeric(lineData@lines[[1]]@Lines[[1]]@coords[,1]))
  expect_equal(as.numeric(ncvar_get(nc, "y")), as.numeric(lineData@lines[[1]]@Lines[[1]]@coords[,2]))
  expect_equal(ncatt_get(nc,varid="x","cf_role")$value,"geometry_x_node")
  expect_equal(ncatt_get(nc,varid="y","cf_role")$value,"geometry_y_node")
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.8")
  expect_equivalent(ncatt_get(nc,varid="instance_name","standard_name")$value,"instance_id")
  expect_equivalent(ncatt_get(nc,varid="x","standard_name")$value,"longitude")
  expect_equivalent(ncatt_get(nc,varid="y","standard_name")$value,"latitude")
  expect_equivalent(ncatt_get(nc,varid="coordinate_index","geom_coordinates")$value,"x y")
  expect_equal(ncatt_get(nc,varid="coordinate_index",attname = "start_index")$value,1)
  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("multiLine data works", {
  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  firstMultiStart<-1
  firstMultiStop<-nc$dim$coordinate_index$vals[which(nc$dim$coordinate_index$vals==-1)[1]-1]
  firstMultiCount<-nc$dim$coordinate_index$vals[which(nc$dim$coordinate_index$vals==-1)[1]-1]-firstMultiStart+1
  expect_equal(firstMultiCount, length(lineData@lines[[1]]@Lines[[1]]@coords[,1]))
  expect_equal(sum(lineData@lines[[1]]@Lines[[1]]@coords[,1]),
               sum(ncvar_get(nc,nc$var$x,start = firstMultiStart, count = firstMultiCount)))
  firstFeatureEnd<-ncvar_get(nc, nc$var$coordinate_index_stop)[1]
  secondMultiStart<-nc$dim$coordinate_index$vals[which(nc$dim$coordinate_index$vals==-1)[1]+1]
  secondMultiCount<-nc$dim$coordinate_index$vals[firstFeatureEnd]-secondMultiStart+1
  expect_equal(secondMultiCount, length(lineData@lines[[1]]@Lines[[2]]@coords[,1]))
  expect_equal(sum(lineData@lines[[1]]@Lines[[2]]@coords[,1]),
               sum(ncvar_get(nc,nc$var$x,start = secondMultiStart, count = secondMultiCount)))
  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("multiline data frame works", {
  lineData <- readRDS("data/multiLineData.rds")
  testdata<-as.data.frame(list("name"=c("test_name"), "id"=c(1)))
  lineData <- SpatialLinesDataFrame(lineData, testdata)
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData, instance_names = as.character(lineData@data$name))
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})
