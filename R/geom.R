#'@title Convert sp objects to NetCDF
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
#'Creates a file with point, line or polygon instance data ready for the extended NetCDF-CF timeSeries featuretype format.
#'Will also add attributes if a sp dataframe object is passed in.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'@importFrom sp SpatialLinesDataFrame polygons
#'
#'@export
ToNCDFSG = function(nc_file, geomData = NULL, names = NULL, lats = NULL, lons = NULL,
                    alts=NULL, multiPoint = NULL){

  pointsMode <- FALSE

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

  if(class(geomData) == "SpatialPolygonsDataFrame") {
    attData<-geomData@data
    geomData<-polygons(geomData)
  } else if(class(geomData) == "SpatialLinesDataFrame") {
    attData<-geomData@data
    geomData<-SpatialLinesDataFrame(geomData,data=as.data.frame(names,stringsAsFactors = FALSE))
  } else if(class(geomData) == "SpatialPolygons") {
    geomData<-polygons(geomData)
  } else if(class(geomData) == "SpatialLines") {
    geomData<-SpatialLinesDataFrame(geomData,data=as.data.frame(names,stringsAsFactors = FALSE))
  } else {
    pointsMode <- TRUE
    if(class(multiPoint) != "SpatialPoints" && !is.null(lats)) {
      stop("Did not find supported spatial data.")
    }
  }

  if(exists("attData")) {
    nc_file <- addInstanceData(nc_file, names, attData = attData)
  } else {
    nc_file <- addInstanceData(nc_file, names)
  }

  if(!is.null(geomData)) nc_file <- addGeomData(nc_file, geomData, names)

  if(pointsMode) {
    nc<-nc_open(nc_file, write = TRUE)

    lat_var 		= ncvar_def('lat', 'degrees_north', dim=nc$dim$instance, -999, prec='double', longname = 'latitude of the observation')
    lon_var 		= ncvar_def('lon', 'degrees_east', dim=nc$dim$instance, -999, prec='double', longname = 'longitude of the observation')

    if(!is.null(alts[1])){
      alt_var = ncvar_def('alt', 'm', dim=nc$dim$instance, missval=-999, prec='double', longname='height above mean sea level')
    }

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


