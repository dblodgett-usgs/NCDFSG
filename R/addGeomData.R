#'@title Put geometry data in a NetCDF-CF File
#'
#'
#'@param nc_file A string file path to the nc file to be created. It must already have an instance dimension.
#'@param geomData An object of class \code{SpatialLines} or \code{SpatialPolygons} with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional geometries is not supported.
#'@param instanceDimName A string to name the instance dimension.  Defaults to "instance"

#'@description
#'Creates a file with point, line or polygon instance data ready for the extended NetCDF-CF timeSeries featuretype format.
#'Will also add attributes if a sp dataframe object is passed in.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open ncvar_add nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'
#'@export
addGeomData<-function(nc_file, geomData, instanceDimName = "instance") {

  hole_break_val <- -2
  holes <- FALSE
  multi_break_val <- -1
  multis <- FALSE

  linesMode<-FALSE

  if(class(geomData) == "SpatialLines" || class(geomData) == "SpatialLinesDataFrame") {
    linesMode<-TRUE
  }

  rInd<-1
  cInd<-1
  ragged_ind<-c()
  ragged_index_stop_vals<-c()
  xVals<-c()
  yVals<-c()
  if(!linesMode) {
    for(geom in 1:length(geomData)) {
      for(part in 1:length(geomData@polygons[[geom]]@Polygons)) {
        if(part > 1) {
          if(geomData@polygons[[geom]]@Polygons[[part]]@hole) {
            ragged_ind<-c(ragged_ind,hole_break_val)
            holes <- TRUE
          } else {
            ragged_ind<-c(ragged_ind,multi_break_val)
            multis <- TRUE
          }
          rInd<-rInd+1
        }
        coords<-geomData@polygons[[geom]]@Polygons[[part]]@coords
        cCount<-length(coords[,1])
        xVals<-c(xVals,coords[,1])
        yVals<-c(yVals,coords[,2])
        ragged_ind<-c(ragged_ind,cInd:(cInd+cCount-1))
        cInd<-cInd+cCount
        rInd<-rInd+cCount
      }
      ragged_index_stop_vals<-c(ragged_index_stop_vals, rInd-1)
    }
  } else if(linesMode) {
    for(geom in 1:length(geomData)) {
      for(part in 1:length(geomData@lines[[geom]]@Lines)) {
        if(part > 1) {
          multis <- TRUE
          ragged_ind<-c(ragged_ind,multi_break_val)
          rInd<-rInd+1
        }
        coords<-geomData@lines[[geom]]@Lines[[part]]@coords
        cCount<-length(coords[,1])
        xVals<-c(xVals,coords[,1])
        yVals<-c(yVals,coords[,2])
        ragged_ind<-c(ragged_ind,cInd:(cInd+cCount-1))
        cInd<-cInd+cCount
        rInd<-rInd+cCount
      }
      ragged_index_stop_vals<-c(ragged_index_stop_vals, rInd-1)
    }
  }

  coord_dim<-ncdim_def('coordinates', '', 1:length(xVals), create_dimvar=FALSE)
  xVar <- ncvar_def(name = "x", units = 'degrees_east', dim = coord_dim, prec = "double")
  yVar <- ncvar_def(name = "y", units = 'degrees_north', dim = coord_dim, prec = "double")

  nc <- nc_open(nc_file,write = TRUE)

  coordinate_index_dim<-ncdim_def('coordinate_index', '', 1:length(ragged_ind), create_dimvar = FALSE)
  coordinate_index_var<-ncvar_def(name = 'coordinate_index', units = '', dim = coordinate_index_dim,
                                  longname = "ragged index for coordinates and geometry break values", prec = "integer")

  coordinate_index_stop_var<-ncvar_def(name = 'coordinate_index_stop', units = '', dim = nc$dim[instanceDimName],
                                       longname = "index for last coordinate in each instance geometry", prec = "integer")

  nc <- ncvar_add(nc,coordinate_index_var)
  nc <- ncvar_add(nc,coordinate_index_stop_var)
  nc <- ncvar_add(nc,xVar)
  nc <- ncvar_add(nc,yVar)
  ncvar_put(nc = nc, varid = 'x', vals = xVals)
  ncvar_put(nc = nc, varid = 'y', vals = yVals)
  ncvar_put(nc = nc, varid = 'coordinate_index', vals = ragged_ind)
  ncvar_put(nc = nc, varid = 'coordinate_index_stop', vals = ragged_index_stop_vals)

  ncatt_put(nc = nc, varid = 'x', attname = 'standard_name', attval = 'longitude')
  ncatt_put(nc = nc, varid = 'y', attname = 'standard_name', attval = 'latitude')
  ncatt_put(nc = nc, varid = 'x', attname = 'cf_role', attval = 'geometry_x_node')
  ncatt_put(nc = nc, varid = 'y', attname = 'cf_role', attval = 'geometry_y_node')
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_coordinates', attval = 'x y')
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_dimension', attval = instanceDimName)
  ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'start_index', attval = 1)
  if(linesMode) {
    if (multis) {
      ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'multipart_break_value', attval = multi_break_val)
      ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_type', attval = 'multiline')
    } else {
      ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_type', attval = 'line')
      }
  } else {
    if (holes) ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'hole_break_value', attval = hole_break_val)
    ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'outer_ring_order', attval = 'clockwise')
    ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'closure_convention', attval = 'last_node_equals_first')
    if (multis) {
      ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_type', attval = 'multipolygon')
      ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'multipart_break_value', attval = multi_break_val)
    } else {
      ncatt_put(nc = nc, varid = 'coordinate_index', attname = 'geom_type', attval = 'polygon')
    }
  }
  ncatt_put(nc = nc, varid = 'coordinate_index_stop', attname = 'contiguous_ragged_dimension', attval = 'coordinate_index')
  ncatt_put(nc, 'instance_name','standard_name','instance_id')
  #Important Global Variables
  ncatt_put(nc, 0,'Conventions','CF-1.8')

  nc_close(nc)
  return(nc_file)
}
