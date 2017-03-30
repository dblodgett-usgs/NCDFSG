library(rgdal)
library(ncdf4)

hucTimeseries <- nc_open(system.file('extdata','example_huc_eta.nc', package = 'NCDFSG'))

hucPolygons<-readOGR(dsn = system.file('extdata','example_huc_eta.json', package = 'NCDFSG'),"OGRGeoJSON", stringsAsFactors = FALSE)

file.copy(system.file('extdata','example_huc_eta.nc', package = 'NCDFSG'), "hucPolygons.nc", overwrite = TRUE)

hucPolygons_nc <- ToNCDFSG(nc_file="hucPolygons.nc", geomData = hucPolygons, instance_names = hucPolygons$HUC12, instance_dim_name = "station", variables = hucTimeseries$var)

hucPolygons_ncdf4 <- nc_open(hucPolygons_nc)

hucPolygons_sp <- FromNCDFSG(hucPolygons_nc)

hucPolygons_shp <- writeOGR(hucPolygons_sp, "hucPolygons.shp", layer = "hucPolygons", driver = "ESRI Shapefile")
