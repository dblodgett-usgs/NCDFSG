context("NCDF SG lineData tests")

# data prep.
# library(maptools)
# lineData<-readShapeLines("data/NHDLine")
# i <- sapply(lineData@data, is.factor)
# lineData@data[i] <- lapply(lineData@data[i], as.character)
# saveRDS(lineData,file="data/NHDline_data.rds")

test_that("shapefile line data works", {
  lineData <- readRDS("data/NHDline_data.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData, names = as.character(lineData@data$COMID))
  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  returnLineData<-FromNCDFSG(nc_file)
  i <- sapply(lineData@data, is, class2 = "Date")
  lineData@data[i] <- lapply(lineData@data[i], as.character)
  compareSL(lineData, returnLineData)
  for(name in names(lineData@data)) {
    expect_equal(class(lineData@data[name][[1]]), class(returnLineData@data[name][[1]]))
  }
  for(name in names(lineData@data)) {
    expect_equal(c(lineData@data[name]), c(returnLineData@data[name]))
  }
})
