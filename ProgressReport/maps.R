library(raster); library(ggplot2); library(dplyr)
midstates <- shapefile('c:/users/secor lab/desktop/gis products/chesapeake/midatlantic/matl_states_land.shp')
pot <- midstates[midstates$STATE_ABBR %in% c('MD', 'VA', 'DC', 'DE'),]

pot <- fortify(pot)

stations <- read.csv('p:/obrien/biotelemetry/receivers/md csi receivers.csv',
                     stringsAsFactors = F)
stations <- stations[stations$Status %in% c('Deployed', 'Proposed'),]


png('p:/obrien/biotelemetry/csi/MD Receivers_Group.png',
    width = 950, height = 600)
ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = filter(stations, Status == 'Deployed'),
             aes(Dec.Long, Dec.Lat, color = Group), size = 5) +
  scale_color_manual(values = c('green', 'darkorange', 'blue', 'purple')) +
  geom_point(data = filter(stations, Status == 'Deployed'),
             aes(Dec.Long, Dec.Lat), size = 5, shape = 21) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14))
dev.off()

### Difference between ASMFC and Section 6 receivers
stations <- stations %>% 
  mutate(Group = ifelse(grepl('Cedar|Rt|Pine|Pier', Station), 'CBL - ASMFC',
                 ifelse(Group == 'CBL' & !grepl('Cedar|Rt|Pine|Pier', Station),
                        'CBL - Section 6', Group)))

ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, color = Group), size = 5) +
  scale_color_manual(values = c('darkgreen', 'lightgreen', 'darkorange',
                                'blue', 'purple')) +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat), size = 5, shape = 21) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 18),
                     legend.title = element_text(size = 20),
                     axis.text = element_text(size = 15),
                     axis.title = element_text(size = 18),
                     title = element_text(size = 20))

### Difference between USCG, Humpty, and coastal receiver attachment
stations <- stations %>% 
  mutate(Group = ifelse(grepl('T-|V-|A-', Station), 'CBL - Coastal',
                 ifelse(grepl('Kent', Station) | Station %in% c('Piney Point - A',
                        'Piney Point - C', 'Piney Point - D', 'Cedar Point - A'),
                        'CBL - Humpty Buoy', Group)),
         Group = ifelse(Group == 'CBL', 'CBL - USCG', Group),
         Group = ifelse(Station == 'CBL Pier', 'CBL Pier', Group))

cblcols <- colorRampPalette(c('darkgreen', 'yellow'))(4)

ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, color = Group), size = 5) +
  scale_color_manual(values = c(cblcols[1], cblcols[2], cblcols[4], cblcols[3],
                                'darkorange', 'blue', 'purple')) +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat), size = 5, shape = 21) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 18),
                     legend.title = element_text(size = 20),
                     axis.text = element_text(size = 15),
                     axis.title = element_text(size = 18),
                     title = element_text(size = 20))

