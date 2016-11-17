#'@title Convert sp objects to NetCDF
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param names A character vector of names for the points. If NULL, integers are used.
#'@param geomData An object of class \code{SpatialPoints}, \code{SpatialLines} or
#'\code{SpatialPolygons} with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional geometries is not supported.
#'@param lats Vector of WGS84 latitudes
#'@param lons Vector of WGS84 longitudes
#'@param alts Vector of altitudes assumed to be height above mean sea level
#'
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

  if(is.null(names) || !is.null(geomData)) {
    names<-as.character(c(1:length(geomData)))
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
  } else if(class(geomData) == "SpatialPoints") {
    pointsMode<-TRUE
    xCoords<-geomData@coords[,1]
    yCoords<-geomData@coords[,2]
  } else if(class(geomData) == "SpatialPointsDataFrame") {
    pointsMode<-TRUE
    attData<-geomData@data
    geomData<-geomData@points
  } else if(!is.null(lats)) {
    pointsMode<-TRUE
    xCoords<-lons
    yCoords<-lats
    if(is.null(names)) {
      names<-as.character(c(1:length(xCoords)))
    }
    if(length(yCoords)!=length(names) || length(xCoords)!=length(names)){
      stop('station_names, lats, and lons must all be vectors of the same length')
    }
  } else {
    stop("Did not find supported spatial data.")
  }

  if(!is.null(geomData)) {
    if(length(names)!=length(geomData)) stop('names must be same length as data')
  }

  if(!is.null(alts[1]) && length(alts)!=length(names)) {
    stop('station_names and alts must all be vectors of the same length')
  }

  if(exists("attData")) {
    nc_file <- addInstanceData(nc_file, names, attData = attData)
  } else {
    nc_file <- addInstanceData(nc_file, names)
  }

  if(!pointsMode) nc_file <- addGeomData(nc_file, geomData, names)

  if(pointsMode) nc_file <- addPoints(nc_file, xCoords, yCoords, alts)

  return(nc_file)
}


