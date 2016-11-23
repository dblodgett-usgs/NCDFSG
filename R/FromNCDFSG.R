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
#'@importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame CRS Line Lines SpatialLines SpatialLinesDataFrame
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

  dataFrame <- as.data.frame(list(id=1:nc$var[coord_index_stop_var][[1]]$dim[[1]]$len))

  for(var in nc$var) {
    if(var$ndims==1 && grepl(var$dim[[1]]$name,paste0("^",instance_id,"$")) &&
       !grepl(var$name, paste0("^",coord_index_stop_var,"$"))) {
      dataFrame[var$name] <- ncvar_get(nc, var$name)
    } else if(grepl(var$prec, paste0("^char$")) &&
              (grepl(var$dim[[1]]$name,paste0("^",instance_id,"$")) ||
               grepl(var$dim[[2]]$name,paste0("^",instance_id,"$"))))
      dataFrame[var$name] <- ncvar_get(nc, var$name)
  }
  if(poly) {
    SPGeom <- SpatialPolygonsDataFrame(SpatialPolygons(Srl, proj4string = CRS("+proj=longlat +datum=WGS84")),
                                       dataFrame, match.ID = FALSE)
  } else if(line) {
    SPGeom <- SpatialLinesDataFrame(SpatialLines(Srl, proj4string = CRS("+proj=longlat +datum=WGS84")),
                                    dataFrame, match.ID = FALSE)
  }
}
  return(SPGeom)
}

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

findVarByAtt <- function(nc, attribute, value, strict=TRUE) {
  foundVar<-list()
  for(variable in c(names(nc$var), names(nc$dim))) {
    temp<-try(ncatt_get(nc,variable,attribute)$value)
    if(strict) value<-paste0("^",value,"$")
    if(!is.null(temp) && grepl(value,temp)) {
      foundVar<-append(foundVar,variable)
    }
  }
  return(foundVar)
}

checkNCDF <- function(nc) {

  instance_id<-NULL
  coord_index_var<-NULL
  coord_index_stop_var<-NULL
  multi_break_val<-NULL
  hole_break_val<-NULL

  # Check important global atts
  if(!grepl('CF',ncatt_get(nc,0,'Conventions')$value)) {
    warning('File does not advertise CF conventions, unexpected behavior may result.')}

  # Look for variable with the timeseries_id in it.
  instance_id<-unlist(findVarByAtt(nc, 'cf_role', 'timeseries_id'))
  if(is.null(instance_id)) { stop('A timeseries id variable was not found in the file.') }

  geom_type<-NULL
  geom_type <- try(ncatt_get(nc, coord_index_var, attname = "geom_type")$value)

  if(grepl("multipolygon", geom_type) || grepl("multiline", geom_type)) {
  # Look for 'geom_coordinates' that match variable names.
  coord_index_var<-list()
  for(var in c(names(nc$var), names(nc$dim))) { # need to come back to handle coordinate variables named the same as the dimension.
    coord_index_var<-append(coord_index_var, findVarByAtt(nc, "geom_coordinates", var, strict=FALSE))
  }
  coord_index_var<-unique(coord_index_var)[[1]]

  if(length(coord_index_var)>1) {stop('only one geom_coordinates index is supported, this file has more than one.')}

  if(length(coord_index_var)==0) {
    stop('No geometry coordinates were found in the file but required for geometry.') }

  coord_index_stop_var<-list()
  for(dimen in c(names(nc$dim))) {
    coord_index_stop_var <- append(coord_index_stop_var, findVarByAtt(nc, "contiguous_ragged_dimension", dimen))
  }
  coord_index_stop_var <- unique(coord_index_stop_var)[[1]]

  if(length(coord_index_stop_var)>1) {stop('only one contiquous ragged dimension index is supported, this file has more than one.')}

  try(multi_break_val <- ncatt_get(nc, coord_index_var, 'multipart_break_value')$value)
  try(hole_break_val <- ncatt_get(nc, coord_index_var, 'hole_break_value')$value)
  # Could also implement outer_ring_order and closure convention.
  }

  return(list(instance_id=instance_id,
              coord_index_var=coord_index_var,
              coord_index_stop_var=coord_index_stop_var,
              multi_break_val=multi_break_val,
              hole_break_val=hole_break_val,
              geom_type=geom_type))
}
