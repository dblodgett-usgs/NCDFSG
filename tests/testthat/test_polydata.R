library("ncdf4")

context("NCDF SG point tests")

# data prep.
# library(maptools)
# shapeData<-readShapePoly("tests/testthat/data/Yahara_alb/Yahara_River_HRUs_alb_eq")
# saveRDS(shapeData,file="data/yahara_shapefile_data.rds")

test_that("A whole shapefile can be written", {
  yaharaData <- readRDS("data/yahara_shapefile_data.rds")
  nc_file <- geom_timeSeries(nc_file=tempfile(), geomData = yaharaData)
})
