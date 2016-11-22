#'@title Put attribute / instance data in a NetCDF-CF File
#'
#'
#'@param nc_file A string file path to the nc file to be created. It must already have an instance dimension.
#'@param names A character vector of names for the points.
#'@param attData A \code{data.frame} as included in a spatial dataFrame.

#'@description
#'Creates a NetCDF file with an instance dimension, names, and any passed
#'in attributes from a data frame. Use to create the start of a NetCDF-DSG file.
#'
#'@references
#'https://github.com/bekozi/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open ncvar_add nc_create nc_close ncvar_def ncvar_put ncdim_def
#'
#'@export
addInstanceData <- function(nc_file, names, attData = NULL) {
  n <- length(names)
  # 'instance' is used to be consistent with the CF specification which calls the geometries, or features, instances.
  instance_dim = ncdim_def('instance', '', 1:n, create_dimvar=FALSE)
  strlen_dim = ncdim_def('name_strlen', '', 1:max(sapply(names, nchar)), create_dimvar=FALSE)
  instance_name_var = ncvar_def('instance_name', '', dim=list(strlen_dim, instance_dim), missval=NULL, prec='char', longname='Instance Names')

  vars<-list()

  types<-list(numeric="double", integer = "integer", character="char")

  if(!is.null(attData)) {
    for(colName in names(attData)) {
      if(grepl(class(attData[colName][[1]]), "character")) {
        charDimLen<-max(sapply(attData[colName][[1]], nchar))
      }
    }
    if(exists("charDimLen")) {
      char_dim = ncdim_def('char', '', 1:charDimLen, create_dimvar=FALSE)
    }
    for(colName in names(attData)) {
      if(grepl(class(attData[colName][[1]]), "character")) {
        vars<-c(vars, list(ncvar_def(name=colName, units = "unknown", dim = list(char_dim, instance_dim),
                                     prec = types[[class(attData[colName][[1]])]])))
      } else {
        vars<-c(vars, list(ncvar_def(name=colName, units = "unknown", dim = instance_dim,
                                     prec = types[[class(attData[colName][[1]])]])))
      }
    }
  }

  vars<-c(vars, list(instance_name_var))

  nc <- nc_create(filename = nc_file, vars = vars)

  nc_close(nc)

  nc <- nc_open(nc_file,write = TRUE)

  ncvar_put(nc = nc, varid = 'instance_name', vals = names)

  if(!is.null(attData)) {
    for(colName in names(attData)) {
      ncvar_put(nc = nc, varid = colName, vals = attData[colName][[1]])
    }
  }

  nc_close(nc)

  return(nc_file)
}
