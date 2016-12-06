compareSP <- function(polygonData, returnPolyData) {
  expect_equal(length(polygonData@polygons[[1]]@Polygons), length(returnPolyData@polygons[[1]]@Polygons))
  for(i in 1:length(length(polygonData@polygons[[1]]@Polygons))) {
    expect_equal(as.numeric(returnPolyData@polygons[[1]]@Polygons[[i]]@coords),
                 as.numeric(polygonData@polygons[[1]]@Polygons[[i]]@coords))
    expect_equal(as.numeric(returnPolyData@polygons[[1]]@Polygons[[i]]@ringDir),
                 as.numeric(polygonData@polygons[[1]]@Polygons[[i]]@ringDir))
    # expect_equal(polygonData@polygons[[1]]@Polygons[[i]], returnPolyData@polygons[[1]]@Polygons[[i]]) # checks attributes, not sure it's work testing them.
  }
  expect_equal(polygonData@polygons[[1]]@area, returnPolyData@polygons[[1]]@area)
  # expect_equal(polygonData@polygons[[1]]@plotOrder, returnPolyData@polygons[[1]]@plotOrder) # Don't want to worry about plot order right now.
  expect_equal(polygonData@polygons[[1]]@labpt, returnPolyData@polygons[[1]]@labpt)
  # expect_equal(polygonData@polygons[[1]]@ID, returnPolyData@polygons[[1]]@ID) # maptools 0 indexes others 1 index. Not roundtripping this yet.
}

compareSL <- function(lineData, returnLineData) {
  expect_equal(length(lineData@lines[[1]]@Lines), length(returnLineData@lines[[1]]@Lines))
  for(i in 1:length(length(lineData@lines[[1]]@Lines))) {
    expect_equal(as.numeric(returnLineData@lines[[1]]@Lines[[i]]@coords),
                 as.numeric(lineData@lines[[1]]@Lines[[i]]@coords))
    # expect_equal(lineData@lines[[1]]@Lines[[i]], returnLineData@lines[[1]]@Lines[[i]]) # checks attributes, not sure it's work testing them.
  }
  # expect_equal(lineData@lines[[1]]@ID, returnLineData@lines[[1]]@ID) # maptools 0 indexes others 1 index. Not roundtripping this yet.
}

checkAllPoly <- function(nc, polygonData, ragged_index, stop_indices) {
  i<-0
  j<-0
  for(g in 1:length(polygonData@polygons)) {
    for(p in 1:length(polygonData@polygons[[g]]@Polygons)) {
      if(p>1) {
        i<-i+1
        if(polygonData@polygons[[g]]@Polygons[[p]]@hole) {
          expect_equal(ragged_index[i], -2)
        } else {expect_equal(ragged_index[i], -1) }
      }
      for(c in 1:length(polygonData@polygons[[g]]@Polygons[[p]]@coords[,1])){
        i<-i+1
        j<-j+1
        expect_equal(ragged_index[i],j)
      }
    }
    expect_equal(ragged_index[stop_indices[g]],j)
    expect_equal(stop_indices[g], i)
  }
}
