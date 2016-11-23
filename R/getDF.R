#'@title Get Dataframe from NetCDF-DSG File
#'
#'
#'@param nc A open ncdf4 object.
#'@param instance_var A representative 1-d instance variable.
#'
#'@description
#'Gets instance data from a NetCDF-DSG file and returns it in a \code{data.frame}
#'
#'@export
#'
getDF <- function(nc, instance_var) {
  instance_dim<-nc$var[instance_var][[1]]$dim
  dataFrame <- as.data.frame(list(id=1:nc$var[instance_var][[1]]$dim[[1]]$len))
  for(var in nc$var) {
    if(var$ndims==1 && grepl(var$dim[[1]]$name,instance_dim) &&
       !grepl(var$name, paste0("^",instance_var,"$"))) {
      dataFrame[var$name] <- c(ncvar_get(nc, var$name))
    } else if(grepl(var$prec, paste0("^char$")) &&
              (grepl(var$dim[[1]]$name,instance_dim) ||
               grepl(var$dim[[2]]$name,instance_dim)))
      dataFrame[var$name] <- c(ncvar_get(nc, var$name))
  }
  dataFrame[] <- lapply(dataFrame, make.true.NA)
  return(dataFrame)
}
