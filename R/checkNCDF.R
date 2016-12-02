#'@title Check NetCDF-DSG File
#'
#'
#'@param nc A open ncdf4 object.
#'
#'@description
#'Introspects a netcdf file and tries to interpret it as a NetCDF-DSG file. Returns a named
#'\code{list} containing \code{instance_id} \code{instanceDim} \code{coord_index_var} \code{coord_index_stop_var}
#'\code{multi_break_val} \code{hole_break_val}. If these values aren't found or aren't applicable,
#'they are returned \code{NULL}.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 ncatt_get
#'
#'@export
checkNCDF <- function(nc) {

  instance_id<-NULL
  instanceDim<-NULL
  coord_index_var<-NULL
  coord_index_stop_var<-NULL
  multi_break_val<-NULL
  hole_break_val<-NULL

  # Assume geom_type is point
  geom_type<-"point"

  # Check important global atts
  if(!grepl('CF',ncatt_get(nc,0,'Conventions')$value)) {
    warning('File does not advertise CF conventions, unexpected behavior may result.')}

  # Look for variable with the timeseries_id in it.
  instance_id<-list()
  instance_id<-append(instance_id, findVarByAtt(nc, 'cf_role', 'timeseries_id'))
  instance_id<-append(instance_id, findVarByAtt(nc, 'standard_name', 'instance_id'))
  station_id<-append(instance_id, findVarByAtt(nc, 'standard_name', 'station_id'))

  if(exists("station_id")) { # can use later to make sure station data are handled correctly.
    instance_id <- station_id
  }

  instance_id<-unlist(unique(instance_id))
  if(is.null(instance_id)) { stop('A timeseries id variable was not found in the file.') }
  if(length(instance_id)>1) { stop('multiple timeseries id variables were found.') }

  # Look for 'geom_coordinates' that match variable names.
  coord_index_var<-list()
  for(var in c(names(nc$var), names(nc$dim))) { # need to come back to handle coordinate variables named the same as the dimension.
    coord_index_var<-append(coord_index_var, findVarByAtt(nc, "geom_coordinates", var, strict=FALSE))
  }

  if(length(coord_index_var)>0) {
    coord_index_var<-unlist(unique(coord_index_var))
    if(length(coord_index_var)>1) {stop('only one geom_coordinates index is supported, this file has more than one.')}
    geom_type <- ncatt_get(nc, coord_index_var, attname = "geom_type")$value

    val <- ncatt_get(nc, coord_index_var, 'multipart_break_value')
    if(val$hasatt) multi_break_val <- val$value

    val <- ncatt_get(nc, coord_index_var, 'hole_break_value')
    if(val$hasatt) hole_break_val <- val$value

    # Could also implement outer_ring_order and closure convention.
  }

  if(grepl("point", geom_type)) {
    coord_index_var<-list()
    for(var in c(names(nc$var), names(nc$dim))) { # need to come back to handle coordinate variables named the same as the dimension.
      coord_index_var<-append(coord_index_var, findVarByAtt(nc, "coordinates", var, strict=FALSE))
    }
    coord_index_var<-unique(coord_index_var)[[1]]
  }

  if(grepl("polygon", geom_type) || grepl("line", geom_type)) {

    coord_index_stop_var<-list()
    for(dimen in c(names(nc$dim))) {
      coord_index_stop_var <- append(coord_index_stop_var, findVarByAtt(nc, "contiguous_ragged_dimension", dimen))
    }
    coord_index_stop_var <- unlist(unique(coord_index_stop_var))

    if(length(coord_index_stop_var)>1) {stop('only one contiquous ragged dimension index is supported, this file has more than one.')}

  }

  if(nc$var[instance_id][[1]]$ndims == 1) {
    instanceDim <- nc$var[instance_id][[1]]$dim[[1]]$name
  } else if(!is.null(coord_index_stop_var)) {
    instanceDim <- nc$var[coord_index_stop_var][[1]]$dim[[1]]$name
  } else if(geom_type == "point" ) {
    latVar<-unlist(findVarByAtt(nc, "standard_name", "latitude", TRUE))
    if(length(latVar)>0) instanceDim <- nc$var[latVar][[1]]$dim[[1]]$name
  }
  if(nc$var[instance_id][[1]]$prec == "char" && nc$var[instance_id][[1]]$ndims == 2 && is.null(instanceDim)) {
    warning("instance dimension is being inferred based on an assumption of dimension order of the character instance_id and may not be correct.")
    instanceDim <- nc$var[instance_id][[1]]$dim[[2]]$name
  }

  return(list(instance_id=instance_id,
              instanceDim=instanceDim,
              coord_index_var=coord_index_var,
              coord_index_stop_var=coord_index_stop_var,
              multi_break_val=multi_break_val,
              hole_break_val=hole_break_val,
              geom_type=geom_type))
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
