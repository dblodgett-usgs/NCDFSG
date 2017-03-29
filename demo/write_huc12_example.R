library(rgdal)
hucPolygons<-readOGR(dsn = system.file('extdata','example_huc_eta.json', package = 'NCDFSG'),"OGRGeoJSON", stringsAsFactors = FALSE)

hucPolygons_nc <- ToNCDFSG(nc_file="hucPolygons.nc", geomData = hucPolygons, instance_names = hucPolygons$HUC12)

hucPolygons_ncdf4 <- nc_open(hucPolygons_nc)

hucPolygons_sp <- FromNCDFSG(hucPolygons_nc)

hucPolygons_shp <- writeOGR(hucPolygons_sp, "hucPolygons.shp", layer = "hucPolygons", driver = "ESRI Shapefile")
