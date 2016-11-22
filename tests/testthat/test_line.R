library(ncdf4)

compareSL <- function(lineData, returnLineData) {
  expect_equal(length(lineData@lines[[1]]@Lines), length(returnLineData@lines[[1]]@Lines))
  for(i in 1:length(length(lineData@lines[[1]]@Lines))) {
    expect_equal(as.numeric(returnLineData@lines[[1]]@Lines[[i]]@coords),
                 as.numeric(lineData@lines[[1]]@Lines[[i]]@coords))
    # expect_equal(lineData@lines[[1]]@Lines[[i]], returnLineData@lines[[1]]@Lines[[i]]) # checks attributes, not sure it's work testing them.
  }
  # expect_equal(lineData@lines[[1]]@ID, returnLineData@lines[[1]]@ID) # maptools 0 indexes others 1 index. Not roundtripping this yet.
}

context("NCDF SG line tests")

test_that("linedata works", {
  lineData <- readRDS("data/lineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(nc$dim$instance$vals,c(1))
  expect_equal(as.numeric(ncvar_get(nc, "x")), as.numeric(lineData@lines[[1]]@Lines[[1]]@coords[,1]))
  expect_equal(as.numeric(ncvar_get(nc, "y")), as.numeric(lineData@lines[[1]]@Lines[[1]]@coords[,2]))
  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,"CF-1.8")
  expect_equivalent(ncatt_get(nc,varid="instance_name","standard_name")$value,"instance_id")
  expect_equivalent(ncatt_get(nc,varid="instance_name","cf_role")$value,"timeseries_id")
  expect_equivalent(ncatt_get(nc,varid="x","standard_name")$value,"geometry x node")
  expect_equivalent(ncatt_get(nc,varid="y","standard_name")$value,"geometry y node")
  expect_equivalent(ncatt_get(nc,varid="coordinate_index","geom_coordinates")$value,"x y")
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
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData, names = as.character(lineData@data$name))
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})
