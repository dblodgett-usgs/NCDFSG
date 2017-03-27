context("Test get proj from netcdf")

test_that("wgs 84 lat lon", {
  crs <- list(grid_mapping_name="latitude_longitude",
              lontidue_of_prime_meridian = 0,
              semi_major_axis = 6378137,
              inverse_flattening = 298)

  prj <- getPrjFromNCDF(crs)

  expect_equal(prj, "+proj=longlat +a=6378137 +f=0.00335570469798658")
})
