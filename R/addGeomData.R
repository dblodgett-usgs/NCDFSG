#'@title Put geometry data in a NetCDF-CF File
#'
#'
#'@param nc_file A string file path to the nc file to be created. It must already have an instance dimension.
#'@param geomData An object of class \code{SpatialLines} or \code{SpatialPolygons} with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional geometries is not supported.
#'@param names A character vector of names for the points.

#'@description
#'Creates a file with point, line or polygon instance data ready for the extended NetCDF-CF timeSeries featuretype format.
#'Will also add attributes if a sp dataframe object is passed in.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open ncvar_add nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'@importFrom broom tidy
#'
#'@export
addGeomData<-function(nc_file, geomData, names) {

  hole_break_val <- -2
  multi_break_val <- -1

  n <- length(names)

  linesMode<-FALSE

  if(class(geomData) == "SpatialLines" || class(geomData) == "SpatialLinesDataFrame") {
    linesMode<-TRUE
  }

  geomData<-tidy(geomData)

  ids<-unique(geomData$id)

  # coordinate index needs to be the length of the coordinates plus the number of multilines.
  # group contains one value for each unique polygon ring.
  coordinate_index_len <- length(geomData$long) + length(levels(geomData$group)) - n

  # Will contain order and mutlipolygon/hole break values.
  coordinate_index_vals <- 1:coordinate_index_len

  if(coordinate_index_len==length(geomData$long)) { # If there are no multiGeometries or holes.
    coordinate_index_vals <- 1:coordinate_index_len

    id_finder<-function(i, d) {max(which(d==i))} # Finds the last index of the value i.
    # coordinate_index_stop_vals are where each polygon stops in the coordinates.
    coordinate_index_stop_vals <- sapply(ids, id_finder, d = geomData$id, USE.NAMES = FALSE)

  } else {
    # for each id, the piece field increments by one for each piece of the geometry same for holes and multipoly.
    coordinate_index_break_vals <- which(diff(as.numeric(geomData$piece))==1)
    # variable to track the number of hole values.
    extraCoord<-0
    # location to start inserting into the coord_index_vals vector
    startCoord<-1
    # location to start pulling from the geomData.
    startInd<-1
    for(cInd in 1:length(coordinate_index_break_vals)) {
      cIndVal<-coordinate_index_break_vals[cInd] # convenience
      # Put the coordinate index data in place.
      coordinate_index_vals[startCoord:(cIndVal+extraCoord)] <- startInd:cIndVal
      # increment the extraCoord.
      extraCoord <- extraCoord + 1
      if(linesMode) {
        coordinate_index_vals[cIndVal+extraCoord] <- multi_break_val
      } else {
        if(geomData$hole[cIndVal+1]) {
          coordinate_index_vals[cIndVal+extraCoord] <- hole_break_val
        } else {
          coordinate_index_vals[cIndVal+extraCoord] <- multi_break_val
        }
      }
      # Increment start positions.
      startInd <- cIndVal+1
      startCoord <- cIndVal + extraCoord + 1
    }
    # Last set of normal polygons.
    coordinate_index_vals[startCoord:(length(geomData$order) + extraCoord)] <- startInd:length(geomData$order)

    # Finds the last index of the value i this time returning the index into the coordinate_index which includes extraCoords.
    id_finder2<-function(i, d, s) {which(s == max(which(d==i)))}
    # coordinate_index_stop_vals are where each polygon stops in the coordinates.
    coordinate_index_stop_vals <- sapply(ids, id_finder2, d = geomData$id,s=coordinate_index_vals, USE.NAMES = FALSE)
  }

  coord_dim<-ncdim_def('coordinates', '', 1:length(geomData$long), create_dimvar=FALSE)
  xVar <- ncvar_def(name = "x", units = 'degrees_east', dim = coord_dim, prec = "double")
  yVar <- ncvar_def(name = "y", units = 'degrees_north', dim = coord_dim, prec = "double")

  xVals <- geomData$long
  yVals <- geomData$lat

  nc <- nc_open(nc_file,write = TRUE)

  coordinate_index_dim<-ncdim_def('coordinate_index', '', 1:coordinate_index_len, create_dimvar = FALSE)
  coordinate_index_var<-ncvar_def(name = 'coordinate_index', units = '', dim = coordinate_index_dim,
                                  longname = "index for coordinates and geometry break values", prec = "integer")

  coordinate_index_stop_var<-ncvar_def(name = 'coordinate_index_stop', units = '', dim = nc$dim$instance,
                                       longname = "index for last coordinate in each instance geometry", prec = "integer")

  nc <- ncvar_add(nc,coordinate_index_var)
  nc <- ncvar_add(nc,coordinate_index_stop_var)
  nc <- ncvar_add(nc,xVar)
  nc <- ncvar_add(nc,yVar)
  ncvar_put(nc = nc, varid = 'x', vals = xVals)
  ncvar_put(nc = nc, varid = 'y', vals = yVals)
  ncvar_put(nc = nc, varid = 'coordinate_index', vals = coordinate_index_vals)
  ncvar_put(nc = nc, varid = 'coordinate_index_stop', vals = coordinate_index_stop_vals)

  ncatt_put(nc = nc, varid = 'x', attname = 'standard_name', attval = 'geometry x node')
  ncatt_put(nc = nc, varid = 'y', attname = 'standard_name', attval = 'geometry y node')
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_coordinates', attval = 'x y')
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'multipart_break_value', attval = multi_break_val)
  if(linesMode) {
    ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_type', attval = 'multiline')
  } else {
    ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'hole_break_value', attval = hole_break_val)
    ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'outer_ring_order', attval = 'anticlockwise')
    ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'closure_convention', attval = 'last_node_equals_first')
    ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_type', attval = 'multipolygon')
  }
  ncatt_put(nc = nc, varid = 'coordinate_index_stop', attname = 'contiguous_ragged_dimension', attval = 'coordinate_index')
  ncatt_put(nc, 'instance_name', 'cf_role', 'timeseries_id')
  ncatt_put(nc, 'instance_name','standard_name','instance_id')
  #Important Global Variables
  ncatt_put(nc, 0,'Conventions','CF-1.8')

  nc_close(nc)
  return(nc_file)
}
