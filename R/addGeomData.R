#'@title Put geometry data in a NetCDF-CF File
#'
#'
#'@param nc_file A string file path to the nc file to be created. It must already have
#'an instance dimension.
#'@param geomData An object of class \code{SpatialLines} or \code{SpatialPolygons} with
#'WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional geometries is not supported.
#'@param instance_dim_name A string to name the instance dimension.  Defaults to "instance"
#'@param variables A character vector of variable names that the geometry data
#'container variable name will be added to.

#'@description
#'Creates a file with point, line or polygon instance data ready for the extended NetCDF-CF timeSeries featuretype format.
#'Will also add attributes if a sp dataframe object is passed in.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open ncvar_add nc_close ncvar_def ncvar_put ncatt_put ncdim_def nc_create
#'
#'@export
addGeomData<-function(nc_file, geomData, instance_dim_name, variables = c()) {

  node_dim_name <- pkg.env$node_dim_name

  linesMode <- FALSE
  pointsMode <- FALSE

  if(class(geomData) == "SpatialLines" || class(geomData) == "SpatialLinesDataFrame") {
    linesMode<-TRUE
  }

  if(class(geomData) == "SpatialPoints" || class(geomData) == "SpatialPointsDataFrame") {
    pointsMode <- TRUE
    xCoords<-geomData@coords[,1]
    yCoords<-geomData@coords[,2]
  }

  holes <- FALSE
  multis <- FALSE

  if(pointsMode) {
    ids <- attributes(geomData@coords)$dimnames[[1]]
    uIds <- unique(ids)
    node_count <- c(1:length(uIds))
    if(length(ids) != length(uIds)) {
      multis <- TRUE
      for(i in 1:length(uIds)) {
        node_count[i] <- length(which(ids == uIds[i]))
      }
    } else {
      node_dim_name <- pkg.env$instance_dim_name
    }
    xVals <- geomData@coords[,1]
    yVals <- geomData@coords[,2]
  } else {
    nCoords <- 0
    nParts <- 0
    nGeoms <- length(geomData)
    for(geom in 1:nGeoms) {
      if(linesMode) { gData <- geomData@lines[[geom]]@Lines
      } else { gData <- geomData@polygons[[geom]]@Polygons }
      nParts <- nParts + length(gData)
      for(part in 1:length(gData)) {
        nCoords <- nCoords + length(gData[[part]]@coords[,1])
      }
    }
    part_type <- rep(NA,nParts)
    part_node_count <- part_type
    node_count <- rep(NA, nGeoms)
    xVals <- rep(NA, nCoords)
    yVals <- xVals
    part <- 0
    coord <- 1
  for(geom in 1:nGeoms) {
    nCount <- 0
    if(linesMode) { gData <- geomData@lines[[geom]]@Lines
    } else { gData <- geomData@polygons[[geom]]@Polygons }
    for(gPart in 1:length(gData)) {
      part <- part + 1
      if(gPart > 1) {
        if(!linesMode && gData[[gPart]]@hole) {
          part_type[part] <- pkg.env$hole_val
          holes <- TRUE
        } else {
          part_type[part] <- pkg.env$multi_val
          multis <- TRUE
        }
      } else {
        part_type[part] <-pkg.env$multi_val
      }
      coords<-gData[[gPart]]@coords
      pCount <- length(coords[,1])
      nCount <- nCount + pCount
      part_node_count[part] <-  pCount
      if(linesMode) {
        xVals[coord:(coord+length(coords[,1])-1)] <- coords[,1]
        yVals[coord:(coord+length(coords[,2])-1)] <- coords[,2]
      } else {
        xVals[coord:(coord+length(coords[,1])-1)]<-coords[nrow(coords):1,1]
        yVals[coord:(coord+length(coords[,2])-1)]<-coords[nrow(coords):1,2]
      }
      coord <- coord + length(coords[,1])
    }
    node_count[geom] <- nCount
  }
  }

  node_dim<-ncdim_def(node_dim_name, '', 1:length(xVals), create_dimvar=FALSE)
  xVar <- ncvar_def(name = "x", units = 'degrees_east', dim = node_dim, prec = "double")
  yVar <- ncvar_def(name = "y", units = 'degrees_north', dim = node_dim, prec = "double")

  if(file.exists(nc_file)) {
    new_file <- TRUE
    nc <- nc_open(nc_file,write = TRUE)
    nc <- ncvar_add(nc,xVar)
    nc <- ncvar_add(nc,yVar)
  } else {
    new_file <- FALSE
    nc <- nc_create(nc_file, list(xVar, yVar))
  }

  nc_close(nc)
  nc <- nc_open(nc_file,write = TRUE)


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

  if(pointsMode) {
    if (multis) {
      ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$geom_type_attr_name, attval = 'multipoint')
    } else {
      ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$geom_type_attr_name, attval = 'point')
    }
  } else if(linesMode) {
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
  if(!(pointsMode && !multis)) {
    ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$node_count_attr_name, attval = pkg.env$node_count_var_name)

    instanceDim <- nc$dim[instance_dim_name]
    if(is.null(unlist(instanceDim))) instanceDim <- ncdim_def(instance_dim_name, '', 1:length(node_count), create_dimvar = FALSE)

    node_count_var<-ncvar_def(name = pkg.env$node_count_var_name, units = '', dim = instanceDim,
                              longname = "count of coordinates in each instance geometry", prec = "integer")
    nc <- ncvar_add(nc, node_count_var)
    ncvar_put(nc = nc, varid = pkg.env$node_count_var_name, vals = node_count)
  }

  ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$node_coordinates, attval = 'x y')

  crs <- getGmFromPrj(geomData@proj4string)

  if(length(crs) == 0) {
      crs <- list(grid_mapping_name = "latitude_longitude",
                  semi_major_axis = 6378137,
                  inverse_flattening = 298.257223563,
                  longitude_of_prime_meridian = 0)
      warning("No CRS was found. Assuming WGS84 Lat Lon.")
  }

  if(length(crs) > 0) {
    ncatt_put(nc = nc, varid = pkg.env$geom_container_var_name, attname = pkg.env$crs, attval = pkg.env$crs_var_name)
    crs_var <- ncvar_def(name = pkg.env$crs_var_name, units = '', dim = list())
    ncvar_add(nc, crs_var)
    nc_close(nc)
    nc <- nc_open(nc_file,write = TRUE)
    for(crs_att in names(crs)) ncatt_put(nc = nc, varid = pkg.env$crs_var_name, attname = crs_att, attval = crs[crs_att][[1]])
  }

  if(!pointsMode && (multis || holes)) {
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

  for(var in variables) {
    ncatt_put(nc, var, pkg.env$geometry_container_att_name, pkg.env$geom_container_var_name)
  }

  nc_close(nc)
  return(nc_file)
}
