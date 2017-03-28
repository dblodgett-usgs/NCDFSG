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
#' \dontrun{
#' prj <- "+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +units=m +no_defs"
#' grid_mapping <- getGmFromPrj(prj)
#' }
#'
getGmFromPrj <- function(prj) {
  al <- prepCRS(prj)
  class(prj) <- class(al)
  UseMethod("getGmFromPrj", prj)
}

getGmFromPrj.latitude_longitude <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "latitude_longitude"),
          getGeoDatum_gm(al))
}

getGmFromPrj.albers_conical_equal_area <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "albers_conical_equal_area"),
       lonCentMer_gm(al),
       latProjOrig_gm(al),
       falseEastNorth_gm(al),
       standPar_gm(al),
       getGeoDatum_gm(al))
}

getGmFromPrj.azimuthal_equidistant <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "azimuthal_equidistant"),
          lonProjOrig_gm(al),
          latProjOrig_gm(al),
          falseEastNorth_gm(al),
          getGeoDatum_gm(al))
}

getGmFromPrj.lambert_azimuthal_equal_area <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "lambert_azimuthal_equal_area"),
          latProjOrig_gm(al),
          lonProjOrig_gm(al),
          falseEastNorth_gm(al),
          getGeoDatum_gm(al))
}

getGmFromPrj.lambert_conformal_conic <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "lambert_conformal_conic"),
                    standPar_gm(al),
                    falseEastNorth_gm(al),
                    latProjOrig_gm(al),
                    lonCentMer_gm(al),
                    getGeoDatum_gm(al))
}

getGmFromPrj.lambert_cylindrical_equal_area <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "lambert_cylindrical_equal_area"),
                    lonCentMer_gm(al),
                    oneStandPar_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

getGmFromPrj.mercator <- function(prj) {
  al <- prepCRS(prj)
  if(!is.null(al$k)) {
    gm <- c(list(grid_mapping_name = "mercator"),
                      lonProjOrig_gm(al),
                      scaleFactor_gm(al),
                      falseEastNorth_gm(al),
                      getGeoDatum_gm(al))
  } else {
    gm <- c(list(grid_mapping_name = "mercator"),
                      lonProjOrig_gm(al),
                      oneStandPar_gm(al),
                      falseEastNorth_gm(al),
                      getGeoDatum_gm(al))
  }
}

getGmFromPrj.oblique_mercator <- function(prj) {
  al <- prepCRS(prj)
  #!!!! Check this one out. the oMerc function is a hack !!!!
  gm <- c(list(grid_mapping_name = "oblique_mercator"),
                    latProjOrig_gm(al),
                    lonProjCent_gm(al),
                    scaleFactor_gm(al),
                    oMerc_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

getGmFromPrj.orthographic <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "orthographic"),
                    latProjOrig_gm(al),
                    lonProjOrig_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

# getGmFromPrj.polar_stereographic <- function(prj) {
#   al <- prepCRS(prj)
#   if(!is.null(al$k)) {
#     gm <- c(list(grid_mapping_name = "polar_stereographic"),
#                       latProjOrig_gm(al),
#                       stVertLon_gm(al),
#                       scaleFactor_gm(al),
#                       falseEastNorth_gm(al),
#                       getGeoDatum_gm(al))
#   } else {
#     gm <- c(list(grid_mapping_name = "polar_stereographic"),
#                       latProjOrig_gm(al),
#                       stVertLon_gm(al),
#                       oneStandPar_gm(al),
#                       falseEastNorth_gm(al),
#                       getGeoDatum_gm(al))
#   }
# }

# getGmFromPrj.sinusoidal <- function(prj) {
#   gm <- c(list(grid_mapping_name = "sinusoidal"),
#                     lonProjOrig_gm(al),
#                     falseEastNorth_gm(al),
#                     getGeoDatum_gm(al))
# }

getGmFromPrj.stereographic <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "stereographic"),
                    latProjOrig_gm(al),
                    lonProjOrig_gm(al),
                    scaleFactor_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

getGmFromPrj.transverse_mercator <- function(prj) {
  al <- prepCRS(prj)
  gm <- c(list(grid_mapping_name = "transverse_mercator"),
                    latProjOrig_gm(al),
                    lonProjOrig_gm(al),
                    scaleFactor_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

lonCentMer_gm <- function(al) {
  list(longitude_of_central_meridian = as.numeric(al$lon_0))
}

latProjOrig_gm <- function(al) {
  list(latitude_of_projection_origin = as.numeric(al$lat_0))
}

lonProjOrig_gm <- function(al) {
  list(longitude_of_projection_origin = as.numeric(al$lon_0))
}

falseEastNorth_gm <- function(al) {
  list(false_easting = as.numeric(al$x_0),
  false_northing = as.numeric(al$y_0))
}

standPar_gm <- function(al) {
  if(al$lat_1 != al$lat_2) {
    list(standard_parallel = c(as.numeric(al$lat_1), as.numeric(al$lat_2)))
  } else if(al$lat_1 == al$lat_2) {
    list(standard_parallel = as.numeric(al$lat_1))
  }
}

oneStandPar_gm <- function(al) {
  list(standard_parallel = c(as.numeric(al$lat_ts)))
}

getGeoDatum_gm <- function(al) {
  list(semi_major_axis = as.numeric(al$a),
       inverse_flattening = (1/as.numeric(al$f)))
}

scaleFactor_gm <- function(al) {
  list(scale_factor_at_projection_origin = as.numeric(al$k))
}

oMerc_gm <- function(al) {
  list(azimuth_of_central_line = as.numeric(al$alpha))
}

lonProjCent_gm <- function(al) {
  list(longitude_of_projection_origin = as.numeric(al$lonc))
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
                         aeqd = "azimuthal_equidistant",
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
