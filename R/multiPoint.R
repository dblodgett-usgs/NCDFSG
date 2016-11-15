#'@title Put points in a CF timeSeries featuretype
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param names A character vector of names for the points. If NULL, integers in the range 1:length(points) are used.
#'@param lats Vector of WGS84 latitudes
#'@param lons Vector of WGS84 longitudes
#'@param alts Vector of altitudes assumed to be height above mean sea level
#'@param multiPoint An object of class \code{SpatialPoints} with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that MULTIPOINT Z is not supported altitude must be passed in seperately.
#'@param add_to_existing boolean If TRUE and the file already exists, variables will be added to the existing file.
#'
#'@description
#'Creates point data in the NetCDF-CF timeSeries featuretype format.
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@importFrom ncdf4 nc_create nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'
#'@export
multiPoint_timeSeries = function(nc_file, names = NULL, lats = NULL, lons = NULL, alts=NULL,
                                 multiPoint = NULL, add_to_existing=FALSE){

  #building this with what I think is the minium required as shown here:
  # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/cf-conventions.html#time-series-data

  if(add_to_existing && !file.exists(nc_file)) {
    warning("add_to_existing was TRUE but the file to add to was not found, starting a new file by the gven name.")
    add_to_existing=FALSE
  }

  if(is.null(lats) && class(multiPoint)!="SpatialPoints") {
    stop("Didn't find lat/lon or a SpatialPoints object, one or the other is required.")
  }

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

  station_dim = ncdim_def('station', '', 1:n, create_dimvar=FALSE)
  strlen_dim = ncdim_def('name_strlen', '', 1:max(sapply(names, nchar)), create_dimvar=FALSE)

  #Setup our spatial and time info
  station_var = ncvar_def('station_name', '', dim=list(strlen_dim, station_dim), missval=NULL, prec='char', longname='Station Names')
  lat_var 		= ncvar_def('lat', 'degrees_north', dim=station_dim, -999, prec='double', longname = 'latitude of the observation')
  lon_var 		= ncvar_def('lon', 'degrees_east', dim=station_dim, -999, prec='double', longname = 'longitude of the observation')

  if(!is.null(alts[1])){
    alt_var = ncvar_def('alt', 'm', dim=station_dim, missval=-999, prec='double', longname='height above mean sea level')
  }
  if(!file.exists(nc_file)) {
    if(!is.null(alts[1])){
      nc = nc_create(nc_file, vars = c(list(lat_var, lon_var, alt_var, station_var)))
    } else {
      nc = nc_create(nc_file, vars = c(list(lat_var, lon_var, station_var)))
      nc_close(nc)
    }
  } else {
    nc<-nc_open(nc_file, write = TRUE)
    ncvar_add(nc, lat_var)
    ncvar_add(nc, lon_var)
    ncvar_add(nc, station_var)
    if(!is.null(alts[1])){
      ncvar_add(nc, alt_far)
    }
    nc_close(nc)
  }

  nc<-nc_open(nc_file, write = TRUE)
  #add standard_names
  ncatt_put(nc, 'lat', 'standard_name', 'latitude')
  ncatt_put(nc, 'lon', 'standard_name', 'longitude')

  if(!is.null(alts[1])){
    ncatt_put(nc, 'alt', 'standard_name', 'height')
  }

  ncatt_put(nc, 'station_name', 'cf_role', 'timeseries_id')
  ncatt_put(nc, 'station_name','standard_name','station_id')

  #Important Global Variables
  ncatt_put(nc, 0,'Conventions','CF-1.7')
  # ncatt_put(nc, 0,'featureType','timeSeries')
  # ncatt_put(nc, 0,'cdm_data_type','Station')
  # ncatt_put(nc, 0,'standard_name_vocabulary','CF-1.7')

  #Put data in NC file
  ncvar_put(nc, lat_var, yCoords, count=n)
  ncvar_put(nc, lon_var, xCoords, count=n)

  if(!is.null(alts[1])){
    ncvar_put(nc, alt_var, alts, count=n)
  }

  ncvar_put(nc, station_var, names, count=c(-1,n))

  nc_close(nc)
  return(nc_file)
}
