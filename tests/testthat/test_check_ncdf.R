context("NCDF check NC tests")

test_that("instance var is found right", {
  nc <- nc_open("data/test_int_instance.nc")

  checkVals <- checkNCDF(nc)
  instance_id<-checkVals$instance_id
  instanceDim<-checkVals$instanceDim
  coord_index_var<-checkVals$coord_index_var
  coord_index_stop_var<-checkVals$coord_index_stop_var
  multi_break_val<-checkVals$multi_break_val
  hole_break_val<-checkVals$hole_break_val
  geom_type<-checkVals$geom_type

  expect_equal(instance_id, "instance_name")
  expect_equal(coord_index_var, "coordinate_index")
  expect_equal(instanceDim, "instance")
})

test_that("point", {
  multipointData <- readRDS("data/pointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc <- nc_open(nc_file)

  checkVals <- checkNCDF(nc)
  instance_id<-checkVals$instance_id
  instanceDim<-checkVals$instanceDim
  coord_index_var<-checkVals$coord_index_var
  coord_index_stop_var<-checkVals$coord_index_stop_var
  multi_break_val<-checkVals$multi_break_val
  hole_break_val<-checkVals$hole_break_val
  geom_type<-checkVals$geom_type

  expect_equal(instance_id, "instance_name")
  expect_equal(coord_index_var, "instance_name")
  expect_equal(geom_type, "point")
  expect_equal(instanceDim, "instance")
})

test_that("line", {
  lineData <- readRDS("data/lineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)
  instance_id<-checkVals$instance_id
  instanceDim<-checkVals$instanceDim
  coord_index_var<-checkVals$coord_index_var
  coord_index_stop_var<-checkVals$coord_index_stop_var
  multi_break_val<-checkVals$multi_break_val
  hole_break_val<-checkVals$hole_break_val
  geom_type<-checkVals$geom_type

  expect_equal(instance_id, "instance_name")
  expect_equal(instanceDim, "instance")
  expect_equal(coord_index_var, "coordinate_index")
  expect_equal(geom_type, "line")
  expect_equal(coord_index_stop_var, "coordinate_index_stop")
})

test_that("line", {
  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)
  multi_break_val<-checkVals$multi_break_val
  geom_type<-checkVals$geom_type

  expect_equal(geom_type, "multiline")
  expect_equal(multi_break_val, -1)
})

test_that("multi polygon holes", {
  polygonData <- readRDS("data/multipolygons_holes.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)
  instance_id<-checkVals$instance_id
  instanceDim<-checkVals$instanceDim
  coord_index_var<-checkVals$coord_index_var
  coord_index_stop_var<-checkVals$coord_index_stop_var
  multi_break_val<-checkVals$multi_break_val
  hole_break_val<-checkVals$hole_break_val
  geom_type<-checkVals$geom_type

  expect_equal(instance_id, "instance_name")
  expect_equal(instanceDim, "instance")
  expect_equal(coord_index_var, "coordinate_index")
  expect_equal(geom_type, "multipolygon")
  expect_equal(multi_break_val, -1)
  expect_equal(hole_break_val, -2)
  expect_equal(coord_index_stop_var, "coordinate_index_stop")
})

test_that("multi polygon holes", {
  polygonData <- readRDS("data/polygonData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)
  multi_break_val<-checkVals$multi_break_val
  hole_break_val<-checkVals$hole_break_val
  geom_type<-checkVals$geom_type

  expect_equal(multi_break_val, NULL)
  expect_equal(hole_break_val, NULL)
  expect_equal(geom_type, "polygon")
})

test_that("errors", {
  multipointData <- readRDS("data/pointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc <- nc_open(nc_file, write = TRUE)
  ncatt_put(nc, nc$var$instance_name, "cf_role", "garbage")
  ncatt_put(nc, nc$var$instance_name, "standard_name", "garbage")
  expect_error(checkNCDF(nc), 'A timeseries id variable was not found in the file.')

  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc <- nc_open(nc_file, write = TRUE)
  ncatt_put(nc, nc$var$lat, "cf_role", "timeseries_id")
  expect_error(checkNCDF(nc), 'multiple timeseries id variables were found.')

  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc <- nc_open(nc_file, write = TRUE)
  ncatt_put(nc, 0,"Conventions", "garbage")
  expect_warning(checkNCDF(nc), 'File does not advertise CF conventions, unexpected behavior may result.')

  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file, write = TRUE)
  ncatt_put(nc, nc$var$instance_name, "geom_coordinates", "x y")
  nc_close(nc)
  nc<-nc_open(nc_file, write = TRUE)
  expect_error(checkNCDF(nc), "only one geom_coordinates index is supported, this file has more than one.")

  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file, write = TRUE)
  ncatt_put(nc, nc$var$x, "contiguous_ragged_dimension", "coordinate_index")
  nc_close(nc)
  nc<-nc_open(nc_file, write = TRUE)
  expect_error(checkNCDF(nc), "only one contiquous ragged dimension index is supported, this file has more than one.")

  multipointData <- readRDS("data/pointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc <- nc_open(nc_file, write = TRUE)
  ncatt_put(nc, nc$var$lat, "standard_name", "garbage")
  expect_warning(checkNCDF(nc), "instance dimension is being inferred based on an assumption of dimension order of the character instance_id and may not be correct.")
})
