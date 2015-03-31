library(rgdal); library(ggvis); library(dplyr)

## Load shapefiles. Shapefiles downloaded from USGS.
york <- readOGR('p:/obrien/gis/shapefiles', 'YKPKCLIP')

york <- york[york$PERMANENT_ %in%
            c("120007612", "120007614", "128576612", "143959607"),]

york@data$ID <- ifelse(york$PERMANENT_ %in% c(128576612, 143959607), 'yk',
                       ifelse(york$PERMANENT_ == 120007614, 'pk', 'mp'))
york <- spChFIDs(york, paste0(york@data$ID, row.names(york)))

## fortify() to turn the object into a data frame suitable for 
# plotting with ggplot2
yk.df <- ggplot2::fortify(york)

## filter() out the points I don't want to have plotted.
yk.plot <- filter(yk.df, grepl('yk', group))
pk.plot <- filter(yk.df, grepl('pk', group))

## Detection data
pass.dat <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                     header = T, stringsAsFactors = F)
pass.dat$Detections <- as.factor(pass.dat$Detections)
pass.dat <- pass.dat %>% mutate(pred.growth = TelemetryR::sturgrow(Temp, Sal, DO.pct),
         river = substr(Site.ID, 1, 2))
yk.dat <- filter(pass.dat, grepl('YK', Site.ID))



ggvis(pass.dat, x = ~DD.Long, y = ~DD.Lat) %>%
  filter(Cruise == eval(input_select(choices = unique(pass.dat$Cruise)))) %>%
  layer_points(fill = ~pred.growth,
               size := 150) %>%
  scale_numeric('fill', range=c('blue','pink')) %>%
  layer_points(size = ~Detections) %>%
  scale_ordinal('size', domain = c(0, 1)) %>%
  layer_paths(x = ~long, y = ~lat, data = group_by(yk.df, group)) %>% 
  add_legend('fill') %>%
  add_legend('size', properties = legend_props(
    legend = list(y = 50))) %>% 
  set_options(duration = 0)
