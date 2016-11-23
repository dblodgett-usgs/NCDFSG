#'@title Convert sp objects to NetCDF
#'
#'
#'@param nc_file A string file path to the nc file to be read.
#'
#'@description
#'Attemps to convert a NetCDF-CF DSG Simple Geometry file into an sp object.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#'@importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame CRS Line Lines SpatialLines SpatialLinesDataFrame SpatialPointsDataFrame
#'
#'@export
FromNCDFSG = function(nc_file) {

  nc <- nc_open(nc_file)

  checkVals <- checkNCDF(nc)

  instance_id<-checkVals$instance_id
  coord_index_var<-checkVals$coord_index_var
  coord_index_stop_var<-checkVals$coord_index_stop_var
  multi_break_val<-checkVals$multi_break_val
  hole_break_val<-checkVals$hole_break_val
  geom_type<-checkVals$geom_type

  line<-FALSE; poly<-FALSE; point<-FALSE
  if(grepl("multipolygon", geom_type)) { poly<-TRUE
  } else if(grepl("multiline", geom_type)) { line<-TRUE
  } else point <- TRUE

  if(point) {
    point_data_var <- strsplit(ncatt_get(nc, coord_index_var, attname = "coordinates")$value, " ")[[1]]
    point_data <- matrix(c(ncvar_get(nc, point_data_var[1]),
                           ncvar_get(nc, point_data_var[2])), ncol=2)
    dataFrame <- getDF(nc, point_data_var)
    SPGeom <- SpatialPointsDataFrame(point_data, proj4string = CRS("+proj=longlat +datum=WGS84"),
                                     data = dataFrame, match.ID = FALSE)
  } else {
    node_data <- strsplit(ncatt_get(nc, coord_index_var, attname = "geom_coordinates")$value, " ")[[1]]
    stop_inds <- ncvar_get(nc, coord_index_stop_var)
    instance_names <- ncvar_get(nc, instance_id)
    start_ind <- 1
    Srl <- list()
    ragged_ind_var<-ncvar_get(nc, coord_index_var)
    for(geom in 1:length(stop_inds)) {
      stop_ind <- stop_inds[geom]
      ragged_inds <- ragged_ind_var[start_ind:stop_ind]
      breaks <- sort(c(which(ragged_inds == hole_break_val), which(ragged_inds == multi_break_val)))
      multi_hole <- rep(FALSE, length(breaks))
      multi_hole[which(ragged_inds[breaks] == hole_break_val)] <- TRUE
      srl <- list()
      coords_start <- 1
      hole<-FALSE # First is always a polygon, not a hole?
      if(length(breaks) > 0) {
        for(part in 1:length(breaks)) {
          coords_count <- ragged_inds[breaks[part]-1] - coords_start + 1
          if(poly) {
            srl <- append(srl, getPolysrl(nc, node_data, coords_start, coords_count, hole))
          } else if(line) {
            srl <- append(srl, getLinesrl(nc, node_data, coords_start, coords_count))
          }
          coords_start <- ragged_inds[breaks[part]+1] # Increments to the next position where there is an index.
          hole<-multi_hole[part] # Indicates that a hole is coming next.
        }
      }
      coords_count <- ragged_inds[stop_ind]-coords_start+1
      if(poly) { # This could be refactored along with the two functions declared below (getPolysrl and getLinesrl)
        srl <- append(srl, getPolysrl(nc, node_data, coords_start, coords_count, hole))
        Srl <- append(Srl, Polygons(srl, as.character(geom)))
      }  else if(line) {
        srl <- append(srl, getLinesrl(nc, node_data, coords_start, coords_count))
        Srl <- append(Srl, Lines(srl, as.character(geom)))
      }
    }
    dataFrame <- getDF(nc, coord_index_stop_var)
    if(poly) {
      SPGeom <- SpatialPolygonsDataFrame(SpatialPolygons(Srl, proj4string = CRS("+proj=longlat +datum=WGS84")),
                                         dataFrame, match.ID = FALSE)
    } else if(line) {
      SPGeom <- SpatialLinesDataFrame(SpatialLines(Srl, proj4string = CRS("+proj=longlat +datum=WGS84")),
                                      dataFrame, match.ID = FALSE)
    }
  }
  nc_close(nc)
  return(SPGeom)
}

# found here: http://stackoverflow.com/questions/26220913/replace-na-with-na
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}

getLinesrl <- function(nc, node_data, coords_start, coords_count) {
  coords <- matrix(c(ncvar_get(nc, node_data[1], coords_start, coords_count),
                     (ncvar_get(nc, node_data[2], coords_start, coords_count))),ncol = 2) # Assuming canonical axis order here!!!
  srl<-Line(coords)
  dimnames(srl@coords) <- list(NULL, c("x", "y"))
  return(srl)
}
# The only thing different between these functions is the call to Line or Polygon. Maybe combine?
getPolysrl <- function(nc, node_data, coords_start, coords_count, hole) {
  coords <- matrix(c(ncvar_get(nc, node_data[1], coords_start, coords_count),
                     (ncvar_get(nc, node_data[2], coords_start, coords_count))),ncol = 2) # Assuming canonical axis order here!!!
  srl<-Polygon(coords, hole=hole)
  dimnames(srl@coords) <- list(NULL, c("x", "y"))
  return(srl)
}
