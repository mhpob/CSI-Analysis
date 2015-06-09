library(gdistance); library(raster); library(rgdal); library(dplyr)

york <- readOGR('c:/users/secor lab/desktop/gis products/york_pamunkey creation',
                'YKPKCLIP')
york <- york[york$PERMANENT_ %in%
            c("120007612", "120007614", "128576612", "143959607"),]

york@data$ID <- ifelse(york$PERMANENT_ %in% c(128576612, 143959607), 'yk',
                       ifelse(york$PERMANENT_ == 120007614, 'pk', 'mp'))
york <- spChFIDs(york, paste0(york@data$ID, row.names(york)))

# Create raster of York River shapefile
ras.back <- raster(extent(bbox(york[grepl('pk',york@data$ID),])),
                   resolution = 1/2160, #5 arc-second grids = 720, 10 = 360
                   vals = 1,
                   crs = proj4string(york))
ras.pk <- rasterize(york[york$ID %in% c('pk', 'yk'),], ras.back)


pk.trans16 <- transition(ras.pk, transitionFunction = function(x){1}, 16)
pk.geo16 <- geoCorrection(pk.trans16, type = 'c')

act.dat <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                     header = T, stringsAsFactors = F)

act.dat <- act.dat %>%
  select(Site.ID, DD.Long, DD.Lat) %>%
  group_by(Site.ID) %>%
  summarize(long = mean(DD.Long),
            lat = mean(DD.Lat)) %>%
  filter(grepl('PK', Site.ID))

row.names(act.dat) <- data.frame(act.dat)[, 1]
act.dat <- data.frame(act.dat)[, 2:3]
act.dat <- as.matrix(act.dat, ncol = 2)

## Need to find distance to mean YK32B
distances <- costDistance(pk.geo16, act.dat,  c(-76.79740, 37.52041))

# > distances
#             [,1]
# PK1     1561.726
# PK10   15366.467
# PK11   16893.347
# PK12   18743.897
# PK13   20183.603
# PK14   21797.083
# PK15   23371.783
# PK16   24812.409
# PK17   26262.174
# PK18   27728.233
# PK19   29411.281
# PK2     3245.525
# PK20   30862.525
# PK21   32198.113
# PK22   33842.722
# PK23   35136.385
# PK24   36601.192
# PK25   38082.480
# PK26   39652.584
# PK27   41078.445
# PK28   42738.539
# PK29   43797.559
# PK3     4304.235
# PK30   44849.030
# PK31   46290.020
# PK32   47883.645
# PK33   49327.810
# PK34   50537.601
# PK35   52099.538
# PK36   53650.542
# PK37   55228.561
# PK38   56748.013
# PK39   58145.089
# PK4     5246.759
# PK40   59483.781
# PK41   61037.782
# PK42   62365.693
# PK43   63888.155
# PK44   64829.190
# PK44.5 65424.718
# PK45   66203.241
# PK46   67588.730
# PK47   68881.485
# PK48   70387.931
# PK49    70856.28
# PK5     6845.946
# PK50   71247.991
# PK6     8760.202
# PK7    10681.223
# PK8    12151.137
# PK9    13744.697

# Google Earth paths
path <- list(NULL)
for (i in 46:nrow(act.dat)) {
  temp <- gdistance::shortestPath(pk.geo16, act.dat[i,],
                                  c(-76.79740, 37.52041),
                                  output = "SpatialLines")
  path[[i]] <- temp@lines[[1]]@Lines[[1]]@coords
}

path <- lapply(path, cbind, 0)

for(i in seq(1,length(path))){ 
cat('<?xml version="1.0" encoding="UTF-8"?>\n<kml xmlns="http://earth.google.com/kml/2.1">\n<Document>\n<Placemark>\n<name>Path A</name>\n<LineString>\n<tessellate>1</tessellate>\n<coordinates>\n',
    file = paste0('path',i,'.kml'))
write.table(path[[i]], row.names = F, col.names = F, sep = ',',
            file = paste0('path',i,'.kml'), append = T)
cat('</coordinates>\n</LineString>\n</Placemark>\n</Document>\n</kml>',
    file = paste0('path',i,'.kml'), append = T)
}
