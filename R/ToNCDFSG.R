#'@title Convert sp objects to NetCDF
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param instance_names A character vector of names for the points. If NULL, integers are used.
#'@param geomData An object of class \code{SpatialPoints}, \code{SpatialLines} or
#'\code{SpatialPolygons} with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional geometries is not supported.
#'@param lats Vector of WGS84 latitudes
#'@param lons Vector of WGS84 longitudes
#'
#'@description
#'Creates a file with point, line or polygon instance data ready for the extended NetCDF-CF timeSeries featuretype format.
#'Will also add attributes if a sp dataframe object is passed in.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open ncvar_add nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'@importFrom sp SpatialLinesDataFrame polygons SpatialPoints
#'@importFrom netcdf.dsg write_instance_data
#'
#'@export
ToNCDFSG = function(nc_file, geomData = NULL, instance_names = NULL, lats = NULL, lons = NULL){

  pointsMode <- FALSE

  if(is.null(instance_names) && !is.null(geomData)) {
    if(class(geomData)=="SpatialPoints" || class(geomData)=="SpatialPointsDataFrame") {
      instance_names <- as.character(unique(attributes(geomData@coords)$dimnames[[1]]))
    } else {
      instance_names <- as.character(c(1:length(geomData)))
    }
  }

  if(class(geomData) == "SpatialPolygonsDataFrame") {
    attData<-geomData@data
    geomData<-polygons(geomData)
  } else if(class(geomData) == "SpatialLinesDataFrame") {
    attData<-geomData@data
  } else if(class(geomData) == "SpatialPolygons") {
    geomData<-polygons(geomData)
  } else if(class(geomData) == "SpatialLines") {
    geomData<-SpatialLinesDataFrame(geomData,data=as.data.frame(instance_names,stringsAsFactors = FALSE))
  } else if(class(geomData) == "SpatialPoints") {
    pointsMode<-TRUE
  } else if(class(geomData) == "SpatialPointsDataFrame") {
    pointsMode<-TRUE
    attData<-geomData@data
  } else if(!is.null(lats)) {
    pointsMode<-TRUE
    geomData <- SpatialPoints(as.data.frame(list(x=lons, y=lats)),proj4string = CRS("+proj=longlat +ellps=WGS84"))
    if(is.null(instance_names)) {
      instance_names<-as.character(c(1:length(lats)))
    }
  } else {
    stop("Did not find supported spatial data.")
  }

  if(!pointsMode && !is.null(geomData)) {
    if(length(instance_names)!=length(geomData)) stop('instance_names must be same length as data')
  }

  instanceDimName <- pkg.env$instance_dim_name

  if(exists("attData")) {
    itemp <- sapply(attData, is.factor)
    attData[itemp] <- lapply(attData[itemp], as.character)
    instance_names<-as.data.frame(list(instance_name=instance_names), stringsAsFactors = FALSE)
    attData<-cbind(instance_names,attData)
    nc_file <- write_instance_data(nc_file, attData, instanceDimName)
  } else {
    instance_names<-as.data.frame(list(instance_name=instance_names), stringsAsFactors = FALSE)
    nc_file <- write_instance_data(nc_file, instance_names, instanceDimName)
  }

  nc_file <- addGeomData(nc_file, geomData, instanceDimName)

  return(nc_file)
}


