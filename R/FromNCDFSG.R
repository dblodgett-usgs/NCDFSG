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
    xCoords <- c(ncvar_get(nc, node_data[1]))
    yCoords <- c(ncvar_get(nc, node_data[2]))
    stop_inds <- c(ncvar_get(nc, coord_index_stop_var))
    instance_names <- ncvar_get(nc, instance_id)
    ragged_ind_var<-ncvar_get(nc, coord_index_var)
    breaks <- sort(c(which(ragged_ind_var == hole_break_val),
                     which(ragged_ind_var == multi_break_val)))
    multi_hole <- rep(FALSE, length(breaks))
    multi_hole[which(ragged_ind_var[breaks] == hole_break_val)] <- TRUE
    start_ind <- 1
    Srl <- list()
    coord_start <- 1
    for(geom in 1:length(stop_inds)) {
      stop_ind <- stop_inds[geom]
      break_ind<-breaks[which(start_ind < breaks & stop_ind > breaks)]
      srl <- list()
      hole<-FALSE # First is always a polygon, not a hole?
      if(length(break_ind) > 0) {
        for(part in 1:length(break_ind)) {
          coord_stop <- ragged_ind_var[break_ind[part]-1]
          coords <- matrix(c(xCoords[coord_start:coord_stop],yCoords[coord_start:coord_stop]),ncol=2)
          if(poly) {
            tsrl<-Polygon(coords, hole=hole)
          } else if(line) {
            tsrl<-Line(coords)
          }
          dimnames(tsrl@coords) <- list(NULL, c("x", "y"))
          srl <- append(srl, tsrl)
          coord_start <- ragged_ind_var[break_ind[part]+1] # Increments to the next position where there is an index.
          hole<-multi_hole[part] # Indicates that a hole is coming next.
        }
      }
      coord_stop <- ragged_ind_var[stop_ind]
      coords <- matrix(c(xCoords[coord_start:coord_stop],yCoords[coord_start:coord_stop]),ncol=2)
      coord_start<-coord_stop+1
      if(poly) {
        tsrl<-Polygon(coords, hole=hole)
      } else if(line) {
        tsrl<-Line(coords)
      }
      dimnames(tsrl@coords) <- list(NULL, c("x", "y"))
      srl <- append(srl, tsrl)
      if(poly) {
        Srl <- append(Srl, Polygons(srl, as.character(geom)))
      }  else if(line) {
        Srl <- append(Srl, Lines(srl, as.character(geom)))
      }
      start_ind <- stop_ind + 1
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
