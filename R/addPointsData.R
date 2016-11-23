#'@title Add points to a NetCDF-DSG File.
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param xCoords Vector of x coordinates in WGS84
#'@param yCoords Vector of y coordinates in WGS84
#'@param alts Vector of altitudes assumed to be height above mean sea level
#'
#'@description
#'Adds x and y coordinates to a NetCDF-DSG dataset following the timeSeries featureType pattern.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open ncvar_add nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'
#'@export
addPoints<-function(nc_file,xCoords,yCoords,alts=NULL) {
  nc<-nc_open(nc_file, write = TRUE)
  n<-length(xCoords)
  lat_var 		= ncvar_def('lat', 'degrees_north', dim=nc$dim$instance, -999, prec='double', longname = 'latitude of the observation')
  lon_var 		= ncvar_def('lon', 'degrees_east', dim=nc$dim$instance, -999, prec='double', longname = 'longitude of the observation')

  if(!is.null(alts[1])){
    alt_var = ncvar_def('alt', 'm', dim=nc$dim$instance, missval=-999, prec='double', longname='height above mean sea level')
  }

  nc <- ncvar_add(nc, lat_var)
  nc <- ncvar_add(nc, lon_var)

  if(!is.null(alts[1])){
    nc <- ncvar_add(nc, alt_var)
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
  ncatt_put(nc, 'instance_name','coordinates','lon lat')


  #Important Global Variables
  ncatt_put(nc, 0,'Conventions','CF-1.7')
  nc_close(nc)
  return(nc_file)
}
