#'@title Convert NetCDF to sp objects
#'
#'
#'@param nc_file A string file path to the nc file to be read.
#'
#'@description
#'Attemps to convert a NetCDF-CF DSG Simple Geometry file into an sp object.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#'@importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame CRS Line Lines SpatialLines SpatialLinesDataFrame SpatialPointsDataFrame
#'@importFrom netcdf.dsg read_instance_data
#'
#'@export
FromNCDFSG = function(nc_file) {

  nc <- nc_open(nc_file)

  checkVals <- checkNCDF(nc)

  instance_id<-checkVals$instance_id
  instanceDim<-checkVals$instanceDim
  geom_container <- checkVals$geom_container
  variable_list <- checkVals$variable_list
  crs <- checkVals$crs

  line<-FALSE; poly<-FALSE; point<-FALSE
  if(grepl("polygon", geom_container$geom_type)) { poly<-TRUE
  } else if(grepl("line", geom_container$geom_type)) { line<-TRUE
  } else point <- TRUE

  xCoords <- c(ncvar_get(nc, geom_container$x))
  yCoords <- c(ncvar_get(nc, geom_container$y))

  if(length(crs) == 0) {
    prj <- "+proj=longlat +datum=WGS84"
  } else {
    prj <- getPrjFromNCDF(crs)
  }

  if(point) {
    point_data <- matrix(c(xCoords,
                           yCoords), ncol=2)
    dataFrame <- read_instance_data(nc, instanceDim)
    if(geom_container$geom_type == "multipoint") {
      stop("reading multipoint is not supported yet.")
      # This is where handling for multipoint would go.
    }
    SPGeom <- SpatialPointsDataFrame(point_data, proj4string = CRS(prj),
                                     data = dataFrame, match.ID = FALSE)
  } else {
    node_count <- c(ncvar_get(nc, geom_container$node_count))
    if(!is.null(instance_id)) {
      instance_names <- ncvar_get(nc, instance_id)
    } else {
      instance_names <- as.character(c(1:length(node_count)))
    }
    if(is.character(geom_container$part_node_count)) {
      part_node_count <- ncvar_get(nc, geom_container$part_node_count)
    } else {
      part_node_count <- node_count
    }
    if(is.character(geom_container$part_type)) {
      part_type <- ncvar_get(nc, geom_container$part_type)
    } else {
      part_type <- rep(pkg.env$multi_val, length(part_node_count))
    }

    node_start <- 1
    geom_node_stop <- 0
    pInd <- 1
    Srl <- list()
    for(geom in 1:length(node_count)) {

      geom_node_stop <- geom_node_stop + node_count[geom]

      srl <- list()

      while(node_start < geom_node_stop) {
        part_node_stop <- node_start + part_node_count[pInd] - 1

        if(part_type[pInd] == pkg.env$hole_val) { hole <- TRUE
        } else { hole <- FALSE }

        coords <- matrix(c(xCoords[node_start:part_node_stop],yCoords[node_start:part_node_stop]),ncol=2)

        if(poly) { tsrl<-Polygon(coords, hole=hole)
        } else if(line) { tsrl<-Line(coords) }

        dimnames(tsrl@coords) <- list(NULL, c("x", "y"))

        srl <- append(srl, tsrl)

        node_start <- node_start + part_node_count[pInd]; pInd <- pInd + 1
      }
      if(poly) {
        Srl <- append(Srl, Polygons(srl, as.character(geom)))
      }  else if(line) {
        Srl <- append(Srl, Lines(srl, as.character(geom)))
      }
    }
    dataFrame <- read_instance_data(nc, instanceDim)

    for(varName in names(dataFrame)) {
      if(!varName %in% variable_list) {
        dataFrame[varName] <- NULL
      }
    }

    if(poly) {
      SPGeom <- SpatialPolygonsDataFrame(SpatialPolygons(Srl, proj4string = CRS("+proj=longlat +datum=WGS84")),
                                         dataFrame, match.ID = FALSE)
    } else if(line) {
      SPGeom <- SpatialLinesDataFrame(SpatialLines(Srl, proj4string = CRS("+proj=longlat +datum=WGS84")),
                                      dataFrame, match.ID = FALSE)
    }
  }
  nc_close(nc)
  return(SPGeom)
}
