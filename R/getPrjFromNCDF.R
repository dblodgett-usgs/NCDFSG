#' Get projection from NetCDF-CF Grid Mapping
#'
#' This function takes a NetCDF-CF projection container and returns
#' a proj4 string.
#'
#' @param grid_mapping_atts A list of attributes of the grid mapping variable
#' as returned by ncdf or ncdf4's get attributes functions.
#'
#' @return A proj4 string for use with the sp CRS function.
#'
#' @export
#'
#' @examples
#' crs <- list(grid_mapping_name="latitude_longitude",
#'             lontidue_of_prime_meridian = 0,
#'             semi_major_axis = 6378137,
#'             inverse_flattening = 298)
#' prj <- getPrjFromNCDF
#'
getPrjFromNCDF <- function(grid_mapping_atts) {
  class(grid_mapping_atts) <- grid_mapping_atts$grid_mapping_name
  UseMethod("getPrjFromNCDF",grid_mapping_atts)
}

getPrjFromNCDF.latitude_longitude <- function(gm) {
  prj <- paste0("+proj=longlat ", getGeoDatum(gm))
}

getGeoDatum <- function(grid_mapping_atts) {

  longitude_of_prime_meridian <- grid_mapping_atts$longitude_of_prime_meridian

  semi_major_axis <- grid_mapping_atts$semi_major_axis

  if (is.null(semi_major_axis)) { semi_major_axis <- 6378137.0 }

  inverse_flattening <- grid_mapping_atts$inverse_flattening

  if (is.null(inverse_flattening)) { inverse_flattening <- 298.257223563 }

  geoDatum <- paste0("+a=", semi_major_axis,
                     " +f=", (1/inverse_flattening))

  # Need to handle longitude of prime meridian.

  return(geoDatum)
}
