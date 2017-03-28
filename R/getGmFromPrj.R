#' Get NetCDF-CF Grid Mapping from Projection
#'
#' This function takes a proj4 string and returns a NetCDF-CF projection container as
#' a named list of attributes.
#'
#' @param prj A proj.4 string as returned from the sp CRS function.
#'
#' @return A named list containing attributes required for that grid_mapping.
#'
#' @importFrom sp CRS
#' @export
#'
#' @examples
#' prj <- "+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +units=m +no_defs"
#' grid_mapping <- getGmFromPrj(prj)
#'
getGmFromPrj <- function(prj) {
  al <- prepCRS(prj)
  class(prj) <- class(al)
  UseMethod("getGmFromPrj", prj)
}

getGmFromPrj.latitude_longitude <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "latitude_longitude"),
          geoD(al))
}

getGmFromPrj.albers_conical_equal_area <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "albers_conical_equal_area"),
       lcm(al),
       lpm(al),
       en(al),
       sp(al),
       geoD(al))
}

lcm <- function(al) {
  list(longitude_of_central_meridian = as.numeric(al$lon_0))
}

lpm <- function(al) {
  list(latitude_of_projection_origin = as.numeric(al$lat_0))
}

en <- function(al) {
  list(false_easting = as.numeric(al$x_0),
  false_northing = as.numeric(al$y_0))
}

sp <- function(al) {
  list(standard_parallel = c(as.numeric(al$lat_1), as.numeric(al$lat_2)))
}

geoD <- function(al) {
  list(semi_major_axis = as.numeric(al$a),
       inverse_flattening = (1/as.numeric(al$f)))
}

prepCRS <- function(prj) {
  checkCRS <- CRS(prj) # verify assumptions but round tripping through CRS.

  args <- unique(unlist(strsplit(prj, " ")))

  argList <- list()

  for(arg in args) {
    a <- unlist(strsplit(sub("\\+", "", arg), "="))
    argList[a[1]] <- a[2]
  }

  cf_proj_lookup <- list(aea = "albers_conical_equal_area",
                         aead = "azimuthal_equidistant",
                         laea = "lambert_azimuthal_equal_area",
                         lcc = "lambert_conformal_conic",
                         cea = "lambert_cylindrical_equal_area",
                         longlat = "latitude_longitude",
                         merc = "mercator",
                         omerc = "oblique_mercator",
                         ortho = "orthographic",
                         stere = "stereographic",
                         tmerc = "transverse_mercator")
  class(argList) <- cf_proj_lookup[unlist(argList["proj"])][[1]]

  return(argList)
}
