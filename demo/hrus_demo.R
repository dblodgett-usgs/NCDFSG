hrus <- readOGR(dsn = "nhru_06/nhru_06.shp", layer = "nhru_06", stringsAsFactors = FALSE)
hrus_nc <- ToNCDFSG(nc_file="nhru_06.nc", geomData = hrus, instance_names = hrus$hru_id_nat)
hrus_sp <- FromNCDFSG(nc_file = "nhru_06.nc")
hrus_shp <- writeOGR(hrus_sp, "nhru_06_nc.shp", layer = "nhru_06_nc", driver = "ESRI Shapefile")
