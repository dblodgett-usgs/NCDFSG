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
  if(grid_mapping_atts$grid_mapping_name=="latitude_longitude") {

    prj <- paste0("+proj=longlat ", getGeoDatum(grid_mapping_atts))

  } else if(grid_mapping_atts$grid_mapping_name=="lambert_conformal_conic") {

    longitude_of_central_meridian <- grid_mapping_atts$longitude_of_central_meridian

    latitude_of_projection_origin <- grid_mapping_atts$latitude_of_projection_origin

    standard_parallel <- grid_mapping_atts$standard_parallel

    false_easting <- grid_mapping_atts$false_easting

    false_northing <- grid_mapping_atts$false_northing

    geoDatum <- getGeoDatum(grid_mapping_atts)

    if(length(standard_parallel==2)) {
      prj <- paste0("+proj=lcc +lat_1=", standard_parallel[1],
                   " +lat_2=", standard_parallel[2],
                   " +lat_0=", latitude_of_projection_origin,
                   " +lon_0=", longitude_of_central_meridian,
                   " +x_0=", false_easting,
                   " +y_0= ", false_northing,
                   geoDatum)
    } else {
      prj <- paste("+proj=lcc +lat_1=", standard_parallel[1],
                   " +lat_2=", standard_parallel[1],
                   " +lat_0=", latitude_of_projection_origin,
                   " +lon_0=", longitude_of_central_meridian,
                   " +x_0=", false_easting,
                   " +y_0= ", false_northing,
                   geoDatum)
    }
  } else {
    # if all else fails, we have to assume EPSG:4326 Lat/Lon.
    prj <- "+init=epsg:4326"
  }
  return(prj)
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
