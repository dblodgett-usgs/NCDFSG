#'@title Put polygons in a CF timeSeries featuretype
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param names A character vector of names for the points. If NULL, integers in the range 1:length(points) are used.
#'@param polygonData An object of class \code{SpatialPolygons} with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional polygons is not supported.
#'@param add_to_existing boolean If TRUE and the file already exists, variables will be added to the existing file.
#'
#'@description
#'Creates a file with polygon instance data in the NetCDF-CF timeSeries featuretype format.
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@importFrom ncdf4 nc_create nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'@importFrom broom tidy
#'@importFrom sp polygons
#'
#'@export

polygon_timeSeries = function(nc_file, polygonData, names = NULL, add_to_existing=FALSE){

  hole_break_val <- -2
  multi_break_val <- -1

  if(add_to_existing && !file.exists(nc_file)) {
    warning("add_to_existing was TRUE but the file to add to was not found, starting a new file by the gven name.")
    add_to_existing=FALSE
  }

  if(is.null(names)) {
    names<-as.character(c(1:length(polygons(polygonData))))
  }

  n<-length(names)

  polygonData<-tidy(polygonData)

  ids<-unique(polygonData$id)

  id_finder<-function(i, d) {max(which(d==i))} # Finds the last index of the value i.
  # coordinate_index_stop_vals are where each polygon stops in the coordinates.
  coordinate_index_stop_vals <- sapply(ids, id_finder, d = polygonData$id, USE.NAMES = FALSE)

  # coordinate index needs to be the length of the coordinates plus the number of multipolygons and hole.
  # group contains one value for each unique polygon ring.
  coordinate_index_len <- length(polygonData$long) + length(levels(polygonData$group)) - n

  # Will contain order and mutlipolygon/hole break values.
  coordinate_index_vals <- 1:coordinate_index_len

  if(coordinate_index_len==length(polygonData$long)) {
    coordinate_index_vals <- 1:coordinate_index_len
  } else {
    # for each id, the piece field increments by one for each piece of the geometry same for holes and multipoly.
    coordinate_index_break_vals <- which(diff(as.numeric(polygonData$piece))==1)
    # variable to track the number of hole values.
    extraCoord<-0
    # location to start inserting into the coord_index_vals vector
    startCoord<-1
    # location to start pulling from the polygonData.
    startInd<-1
    for(cInd in 1:length(coordinate_index_break_vals)) {
      cIndVal<-coordinate_index_break_vals[cInd] # convenience
      # Put the coordinate index data in place.
      coordinate_index_vals[startCoord:(cIndVal+extraCoord)] <- startInd:cIndVal
      # increment the extraCoord.
      extraCoord <- extraCoord + 1
      # Check if hole of multipoly.
      if(polygonData$hole[cIndVal+1]) {
        coordinate_index_vals[cIndVal+extraCoord] <- hole_break_val
      } else {
        coordinate_index_vals[cIndVal+extraCoord] <- multi_break_val
      }
      # Increment start positions.
      startInd <- cIndVal+1
      startCoord <- cIndVal + extraCoord + 1
    }
    # Last set of normal polygons.
    coordinate_index_vals[startCoord:(length(polygonData$order) + extraCoord)] <- polygonData$order[startInd:length(polygonData$order)]
  }

  # 'instance' is used to be consistent with the CF specification which calls the geometries, or features, instances.
  instance_dim = ncdim_def('instance', '', 1:n, create_dimvar=FALSE)
  strlen_dim = ncdim_def('name_strlen', '', 1:max(sapply(names, nchar)), create_dimvar=FALSE)
  instance_name_var = ncvar_def('instance_name', '', dim=list(strlen_dim, instance_dim), missval=NULL, prec='char', longname='Instance Names')

  coord_dim<-ncdim_def('coordinates', '', 1:length(polygonData$long), create_dimvar=FALSE)
  xVar <- ncvar_def(name = "x", units = 'degrees_east', dim = coord_dim, prec = "double")
  yVar <- ncvar_def(name = "y", units = 'degrees_north', dim = coord_dim, prec = "double")

  xVals <- polygonData$long
  yVals <- polygonData$lat

  coordinate_index_dim<-ncdim_def('coordinate_index', '', 1:coordinate_index_len, create_dimvar = FALSE)
  coordinate_index_var<-ncvar_def(name = 'coordinate_index', units = '', dim = coordinate_index_dim, longname = "index for coordinates and multipolygon and hole break values")

  coordinate_index_stop_var<-ncvar_def(name = 'coordinate_index_stop', units = '', dim = instance_dim, longname = "index for last coordinate in each instance polygon")

  nc <- nc_create(filename = nc_file, vars = list(coordinate_index_var,
                                                  coordinate_index_stop_var,
                                                  instance_name_var, xVar, yVar))
  nc_close(nc)

  nc <- nc_open(nc_file,write = TRUE)

  ncvar_put(nc = nc, varid = 'instance_name', vals = names)
  ncvar_put(nc = nc, varid = 'x', vals = xVals)
  ncvar_put(nc = nc, varid = 'y', vals = yVals)
  ncvar_put(nc = nc, varid = 'coordinate_index', vals = coordinate_index_vals)
  ncvar_put(nc = nc, varid = 'coordinate_index_stop', vals = coordinate_index_stop_vals)

  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_type', attval = 'multipolygon')
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'coordinates', attval = 'x y')
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'multipart_break_value', attval = multi_break_val)
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'hole_break_value', attval = hole_break_val)
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'outer_ring_order', attval = 'anticlockwise')
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'closure_convention', attval = 'last_node_equals_first')

  ncatt_put(nc = nc, varid = 'coordinate_index_stop', attname = 'contiguous_ragged_dimension', attval = 'coordinate_index')

  ncatt_put(nc, 'instance_name', 'cf_role', 'timeseries_id')
  ncatt_put(nc, 'instance_name','standard_name','instance_id')

  #Important Global Variables
  ncatt_put(nc, 0,'Conventions','CF-1.7')
  ncatt_put(nc, 0,'featureType','timeSeries')
  ncatt_put(nc, 0,'cdm_data_type','polygon')
  ncatt_put(nc, 0,'standard_name_vocabulary','CF-1.7')

  nc_close(nc)

  return(nc_file)
}
