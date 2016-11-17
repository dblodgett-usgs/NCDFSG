#'@title Put lines in a CF file
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param names A character vector of names for the points. If NULL, integers are used.
#'@param geomData An object of class \code{SpatialLines} or \code{SpatialPolygons} with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional geometries is not supported.
#'@param lats Vector of WGS84 latitudes
#'@param lons Vector of WGS84 longitudes
#'@param alts Vector of altitudes assumed to be height above mean sea level
#'@param multiPoint An object of class \code{SpatialPoints} with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that MULTIPOINT Z is not supported altitude must be passed in seperately.

#'@description
#'Creates a file with line or polygon instance data ready for the NetCDF-CF timeSeries featuretype format.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_create nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'@importFrom broom tidy
#'@importFrom sp SpatialLinesDataFrame polygons
#'
#'@export

geom_timeSeries = function(nc_file, geomData = NULL, names = NULL,
                           lats = NULL, lons = NULL, alts=NULL,
                           multiPoint = NULL){

  hole_break_val <- -2
  multi_break_val <- -1

  pointsMode <- FALSE
  linesMode <- FALSE

  if(class(geomData) == "SpatialLines") {
    linesMode<-TRUE
  } else if(class(geomData) == "SpatialPolygons") {
    linesMode<-FALSE
  } else if(class(geomData) == "SpatialPolygonsDataFrame") {
    linesMode<-FALSE
    attData<-geomData@data
    geomData<-polygons(geomData)
  }  else { pointsMode <- TRUE }

  if(pointsMode) {
    if(class(multiPoint) != "SpatialPoints" && !is.null(lats)) {
      stop("Did not find supported spatial data.")
    }
  }

  # One last check for points in case they are in addition to other things.
  if(class(multiPoint) == "SpatialPoints" || !is.null(lats)) {
    pointsMode <- TRUE

    if(is.null(lats) && class(multiPoint)=="SpatialPoints") {
      xCoords<-multiPoint@coords[,1]
      yCoords<-multiPoint@coords[,2]
    }

    if(!is.null(lats)) {
      xCoords<-lons
      yCoords<-lats
    }

    if(is.null(names)) {
      names<-as.character(c(1:length(xCoords)))
    }

    n<-length(names)

    if(length(yCoords)!=n || length(xCoords)!=n){
      stop('station_names, lats, and lons must all be vectors of the same length')
    }

    if(!is.null(alts[1]) && length(alts)!=n){
      stop('station_names and alts must all be vectors of the same length')
    }
  } else {
    if(is.null(names)) {
      names<-as.character(c(1:length(geomData)))
    } else {
      if(length(names)!=length(geomData)) stop('names must be same length as data')
    }

    n<-length(names)
  }

  # 'instance' is used to be consistent with the CF specification which calls the geometries, or features, instances.
  instance_dim = ncdim_def('instance', '', 1:n, create_dimvar=FALSE)
  strlen_dim = ncdim_def('name_strlen', '', 1:max(sapply(names, nchar)), create_dimvar=FALSE)
  instance_name_var = ncvar_def('instance_name', '', dim=list(strlen_dim, instance_dim), missval=NULL, prec='char', longname='Instance Names')

  vars<-list()

  types<-list(numeric="double", integer = "integer", character="char")
  if(exists("attData")) {
    for(colName in names(attData)) {
      vars<-c(vars, list(ncvar_def(name=colName, units = "unknown", dim = instance_dim, prec = types[[class(attData[colName][[1]])]])))
    }
  }

  vars<-c(vars, list(instance_name_var))

  nc <- nc_create(filename = nc_file, vars = vars)

  nc_close(nc)

  nc <- nc_open(nc_file,write = TRUE)

  ncvar_put(nc = nc, varid = 'instance_name', vals = names)

  if(exists("attData")) {
    for(colName in names(attData)) {
      ncvar_put(nc = nc, varid = colName, vals = attData[colName][[1]])
    }
  }

  nc_close(nc)

  if(linesMode) {
    geomData<-tidy(SpatialLinesDataFrame(geomData,data=as.data.frame(names,stringsAsFactors = FALSE)))
  } else if(!is.null(geomData)) {
    geomData<-tidy(geomData)
  }

  if(!is.null(geomData)) {

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
      id_finder<-function(i, d, s) {which(s == max(which(d==i)))}
      # coordinate_index_stop_vals are where each polygon stops in the coordinates.
      coordinate_index_stop_vals <- sapply(ids, id_finder, d = geomData$id,s=coordinate_index_vals, USE.NAMES = FALSE)
    }

    coord_dim<-ncdim_def('coordinates', '', 1:length(geomData$long), create_dimvar=FALSE)
    xVar <- ncvar_def(name = "x", units = 'degrees_east', dim = coord_dim, prec = "double")
    yVar <- ncvar_def(name = "y", units = 'degrees_north', dim = coord_dim, prec = "double")

    xVals <- geomData$long
    yVals <- geomData$lat

    coordinate_index_dim<-ncdim_def('coordinate_index', '', 1:coordinate_index_len, create_dimvar = FALSE)
    coordinate_index_var<-ncvar_def(name = 'coordinate_index', units = '', dim = coordinate_index_dim,
                                    longname = "index for coordinates and geometry break values", prec = "integer")

    coordinate_index_stop_var<-ncvar_def(name = 'coordinate_index_stop', units = '', dim = instance_dim,
                                         longname = "index for last coordinate in each instance geometry", prec = "integer")

    nc <- nc_open(nc_file,write = TRUE)

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
    ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'coordinates', attval = 'x y')
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
  }

  if(pointsMode) {
    lat_var 		= ncvar_def('lat', 'degrees_north', dim=instance_dim, -999, prec='double', longname = 'latitude of the observation')
    lon_var 		= ncvar_def('lon', 'degrees_east', dim=instance_dim, -999, prec='double', longname = 'longitude of the observation')

    if(!is.null(alts[1])){
      alt_var = ncvar_def('alt', 'm', dim=instance_dim, missval=-999, prec='double', longname='height above mean sea level')
    }
    nc<-nc_open(nc_file, write = TRUE)
    nc <- ncvar_add(nc, lat_var)
    nc <- ncvar_add(nc, lon_var)

    if(!is.null(alts[1])){
      nc <- ncvar_add(nc, alt_far)
    }

    ncvar_put(nc, lat_var, yCoords, count=n)
    ncvar_put(nc, lon_var, xCoords, count=n)

    if(!is.null(alts[1])){
      ncvar_put(nc, alt_var, alts, count=n)
    }

    #add standard_names
    ncatt_put(nc, 'lat', 'standard_name', 'latitude')
    ncatt_put(nc, 'lon', 'standard_name', 'longitude')

    if(!is.null(alts[1])){
      ncatt_put(nc, 'alt', 'standard_name', 'height')
    }

    ncatt_put(nc, 'instance_name', 'cf_role', 'timeseries_id')
    ncatt_put(nc, 'instance_name','standard_name','station_id')

    #Important Global Variables
    ncatt_put(nc, 0,'Conventions','CF-1.7')
    nc_close(nc)
  }
  return(nc_file)
}
