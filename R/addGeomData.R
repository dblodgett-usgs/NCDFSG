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
addGeomData<-function(nc_file, geomData, instanceDimName) {

  holes <- FALSE
  multis <- FALSE

  linesMode<-FALSE

  if(class(geomData) == "SpatialLines" || class(geomData) == "SpatialLinesDataFrame") {
    linesMode<-TRUE
  }

  node_count <- c()
  part_node_count <- c()
  part_type <- c(pkg.env$multi_val) # First is always an outside ring.
  xVals<-c()
  yVals<-c()
  for(geom in 1:length(geomData)) {
    nCount <- 0
    if(linesMode) { gData <- geomData@lines[[geom]]@Lines
    } else { gData <- geomData@polygons[[geom]]@Polygons }
    for(part in 1:length(gData)) {
      if(part > 1) {
        if(!linesMode && gData[[part]]@hole) {
          part_type <- c(part_type, pkg.env$hole_val)
          holes <- TRUE
        } else {
          part_type <- c(part_type, pkg.env$multi_val)
          multis <- TRUE
        }
      }
      coords<-gData[[part]]@coords
      pCount <- length(coords[,1])
      nCount <- nCount + pCount
      part_node_count <- c(part_node_count, pCount)
      xVals<-c(xVals,coords[,1])
      yVals<-c(yVals,coords[,2])
    }
    node_count <- c(node_count, nCount)
  }

  nc <- nc_open(nc_file,write = TRUE)

  node_dim<-ncdim_def(pkg.env$node_dim_name, '', 1:length(xVals), create_dimvar=FALSE)
  xVar <- ncvar_def(name = "x", units = 'degrees_east', dim = node_dim, prec = "double")
  yVar <- ncvar_def(name = "y", units = 'degrees_north', dim = node_dim, prec = "double")
  nc <- ncvar_add(nc,xVar)
  nc <- ncvar_add(nc,yVar)
  ncvar_put(nc = nc, varid = 'x', vals = xVals)
  ncvar_put(nc = nc, varid = 'y', vals = yVals)
  ncatt_put(nc = nc, varid = 'x', attname = 'standard_name', attval = 'longitude')
  ncatt_put(nc = nc, varid = 'y', attname = 'standard_name', attval = 'latitude')
  ncatt_put(nc = nc, varid = 'x', attname = 'cf_role', attval = 'geometry_x_node')
  ncatt_put(nc = nc, varid = 'y', attname = 'cf_role', attval = 'geometry_y_node')

  geom_container <- ncvar_def(name = pkg.env$geom_container_var_name, units = '', dim = list())
  ncvar_add(nc, geom_container)

  nc_close(nc)
  nc <- nc_open(nc_file,write = TRUE)

  if(linesMode) {
    if (multis) {
      ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$geom_type_attr_name, attval = 'multiline')
    } else {
      ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$geom_type_attr_name, attval = 'line')
    }
  } else {
    if (multis) {
      ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$geom_type_attr_name, attval = 'multipolygon')
    } else {
      ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$geom_type_attr_name, attval = 'polygon')
    }
  }
  ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$node_count_attr_name, attval = pkg.env$node_count_var_name)
  ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$node_coordinates, attval = 'x y')

  node_count_var<-ncvar_def(name = pkg.env$node_count_var_name, units = '', dim = nc$dim[instanceDimName],
                            longname = "count of coordinates in each instance geometry", prec = "integer")
  nc <- ncvar_add(nc, node_count_var)
  ncvar_put(nc = nc, varid = pkg.env$node_count_var_name, vals = node_count)

  if(multis || holes) {
    part_node_count_dim<-ncdim_def(pkg.env$part_dim_name, '', 1:length(part_node_count), create_dimvar = FALSE)
    part_node_count_var<-ncvar_def(name = pkg.env$part_node_count_var_name, units = '', dim = part_node_count_dim,
                                    longname = "count of nodes in each geometry part", prec = "integer")
    nc <- ncvar_add(nc, part_node_count_var)
    ncvar_put(nc = nc, varid = pkg.env$part_node_count_var_name, vals = part_node_count)
    ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$part_node_count_attr_name, attval = pkg.env$part_node_count_var_name)
    if(holes) {
      part_type_var <- ncvar_def(name = pkg.env$part_type_var_name, units = '', dim = part_node_count_dim,
                                 longname = "type of each geometry part", prec = "integer")
      nc <- ncvar_add(nc, part_type_var)
      ncvar_put(nc = nc, varid = pkg.env$part_type_var_name, vals = part_type)
      ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$part_type_attr_name, attval = pkg.env$part_type_var_name)
    }
  }

  ncatt_put(nc, 0,'Conventions', pkg.env$cf_version)

  nc_close(nc)
  return(nc_file)
}
