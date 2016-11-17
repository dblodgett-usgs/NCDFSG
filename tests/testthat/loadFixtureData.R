library(ncdf4)

context("NCDF SG Base Fixture Tests")

# Data Prep - this could be included in tests, but then rgeos would be required for tests.
library(jsonlite)
library(rgeos)
fixtureData<-fromJSON(readLines(system.file('extdata','fixture_wkt.json', package = 'NCDFSG'),warn = FALSE))
multipointData <- readWKT(fixtureData[["2d"]]$multipoint)
pointData <- readWKT(fixtureData[["2d"]]$point)
lineData <- readWKT(fixtureData[["2d"]]$linestring)
multiLineData <- readWKT(fixtureData[["2d"]]$multilinestring)
polygonData <- readWKT(fixtureData[["2d"]]$polygon)
polygon_holeData <- readWKT(fixtureData[["2d"]]$polygon_hole)
multipolygonData <- readWKT(fixtureData[["2d"]]$multipolygon)
multipolygon_holeData <- readWKT(fixtureData[["2d"]]$multipolygon_hole)
multipolygons_holesData <- readWKT(fixtureData[["2d"]]$multipolygons_holes)
multigeometries_polygons_holesData <- readWKT(paste0("GEOMETRYCOLLECTION(",fixtureData[["2d"]]$multipolygons_holes,", ",fixtureData[["2d"]]$multipolygon_hole,")"), id = c("1","2"))

saveRDS(multipointData,file="data/multiPointData.rds")
saveRDS(pointData,file="data/pointData.rds")
saveRDS(lineData, file="data/lineData.rds")
saveRDS(multiLineData, file="data/multiLineData.rds")
saveRDS(polygonData, file="data/polygonData.rds")
saveRDS(polygon_holeData, file="data/polygon_holeData.rds")
saveRDS(multipolygonData, file="data/multipolygonData.rds")
saveRDS(multipolygon_holeData, file="data/multipolygon_holeData.rds")
saveRDS(multipolygons_holesData, file="data/multipolygons_holes.rds")
saveRDS(multigeometries_polygons_holesData, file="data/multigeometries_polygons_holes.rds")

order<-c("point", "linestring", "polygon",
         "multipoint", "multilinestring", "multipolygon",
         "polygon_hole", "multipolygon_hole", "multipolygons_holes")

namesstr<-c("Point (2D)", "LineString (2D)", "Polygon (2D)",
         "MultiPoint (2D)", "MultiLineString (2D)", "MultiPolygon (2D)",
         "Polygon with One Interior Ring (2D)", "MultiPolygon with One Interior Ring (2D)",
         "Multiple MultiPolygons with Interior Rings (2D)")

sink('examples.md')
cat(paste("# Examples - Contiguous Ragged Arrays  \nCreated",Sys.time(),"  \n\n"))

for(geom in 1:length(namesstr)) {
  cat(paste0("## ", namesstr[geom],"  \nWell Known Text (WKT): ```",fixtureData[["2d"]][order[geom]]),"```  \n")
  fileName<-paste0("sample_",order[geom],".nc")
  if(grepl('point',order[geom])) {
    ToNCDFSG(fileName,multiPoint = readWKT(fixtureData[["2d"]][order[geom]]))
  } else {
    ToNCDFSG(fileName,readWKT(fixtureData[["2d"]][order[geom]]))
  }
  cat("Common Data Language (CDL):\n```  \n")
  t<-system(paste0("ncdump ", fileName),intern = TRUE)
  cat(t,sep = "\n")
  cat("  \n```  \n\n")
}
sink()
