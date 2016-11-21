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
#'@importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame CRS
#'
#'@export
FromNCDFSG = function(nc_file) {
  nc <- nc_open(nc_file)

  # Check important global atts
  if(!grepl('CF',ncatt_get(nc,0,'Conventions')$value)) {
    warning('File does not advertise CF conventions, unexpected behavior may result.')}

  # Look for variable with the timeseries_id in it.
  instance_id<-unlist(findVarByAtt(nc, 'cf_role', 'timeseries_id'))
  if(is.null(instance_id)) { stop('A timeseries id variable was not found in the file.') }

  # Look for 'geom_coordinates' that match variable names.
  coord_index_var<-list()
  for(var in c(names(nc$var), names(nc$dim))) { # need to come back to handle coordinate variables named the same as the dimension.
    coord_index_var<-append(coord_index_var, findVarByAtt(nc, "geom_coordinates", var, strict=FALSE))
  }
  coord_index_var<-unique(coord_index_var)[[1]]

  if(length(coord_index_var)>1) {stop('only one geom_coordinates index is supported, this file has more than one.')}

  node_data <- strsplit(ncatt_get(nc, coord_index_var[[1]], attname = "geom_coordinates")$value, " ")[[1]]

  if(length(coord_index_var)==0) { stop('No geometry coordinates were found in the file.') }

  coord_index_stop_var<-list()
  for(dimen in c(names(nc$dim))) {
    coord_index_stop_var <- append(coord_index_stop_var, findVarByAtt(nc, "contiguous_ragged_dimension", dimen))
  }
  coord_index_stop_var <- unique(coord_index_stop_var)[[1]]

  if(length(coord_index_stop_var)>1) {stop('only one contiquous ragged dimension index is supported, this file has more than one.')}

  if(grepl(ncatt_get(nc, coord_index_var, 'geom_type')$value,'^multipolygon$')) {
    multi_break_val <- ncatt_get(nc, coord_index_var, 'multipart_break_value')$value
    hole_break_val <- ncatt_get(nc, coord_index_var, 'hole_break_value')$value
    # Could also implement outer_ring_order and closure convention.
  }

  stop_inds <- ncvar_get(nc, coord_index_stop_var)
  instance_names <- ncvar_get(nc, instance_id)
  start_ind <- 1
  Srl <- list()
  for(geom in 1:length(stop_inds)) {
    name <- instance_names[geom]
    stop_ind <- stop_inds[geom]
    ragged_inds <- ncvar_get(nc, coord_index_var, start_ind, (stop_ind-start_ind+1))
    breaks <- sort(c(which(ragged_inds == hole_break_val), c(which(ragged_inds == multi_break_val))))
    multi_hole <- rep(FALSE, length(breaks))
    multi_hole[which(ragged_inds[breaks] == hole_break_val)] <- TRUE
    extra_inds <- 0
    srl <- list()
    coords_start <- 1
    for(part in 1:length(breaks)) {
      coords_count <- breaks[part]-coords_start
      coords <- matrix(c(ncvar_get(nc, node_data[1], coords_start, coords_count),
                         (ncvar_get(nc, node_data[2], coords_start, coords_count))),ncol = 2) # Assuming canonical axis order here!!!
      coords_start <- breaks[part]+1
      srl <- append(srl, Polygon(coords, hole=multi_hole[part]))
    }
    Srl <- append(Srl, Polygons(srl, as.character(geom)))
  }
  SPolys <- SpatialPolygonsDataFrame(SpatialPolygons(Srl, proj4string = CRS("+proj=longlat +datum=WGS84")),
                                     as.data.frame(instance_names, stringsAsFactors = FALSE), match.ID = FALSE)
  return(SPolys)
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

