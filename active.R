library(TelemetryR); library(rgdal); library(ggplot2); library(dplyr)

## Input -------------------------------------------------------------------
# Load shapefiles. Shapefiles downloaded from USGS.
york <- readOGR('c:/users/secor lab/desktop/gis products/york_pamunkey creation',
                'YKPKCLIP')

york <- york[york$PERMANENT_ %in%
            c("120007612", "120007614", "128576612", "143959607"),]

york@data$ID <- ifelse(york$PERMANENT_ %in% c(128576612, 143959607), 'yk',
                       ifelse(york$PERMANENT_ == 120007614, 'pk', 'mp'))
york <- spChFIDs(york, paste0(york@data$ID, row.names(york)))


yk.df <- fortify(york)

yk.plot <- filter(yk.df, grepl('yk', group))
pk.plot <- filter(yk.df, grepl('pk', group))

# Detection data
pass.dat <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                     header = T, stringsAsFactors = F)
pass.dat$Detections <- as.factor(pass.dat$Detections)
pass.dat <- pass.dat %>% 
  mutate(pred.growth = sturgrow(Temp, Sal, DO.pct),
         river = substr(Site.ID, 1, 2))

# New factor to correctly order cruises
hold <- data.frame(Cruise = levels(factor(pass.dat$Cruise)),
                   d.range = NA)
for(i in seq(1, dim(hold)[1],1)){
  hold$d.range[i] <- paste(range(filter(pass.dat, Cruise == hold[i, 1])$Date),
                        collapse = ' - ')
}
hold$d.range <- factor(hold$d.range, levels = hold$d.range, ordered = T)
pass.dat <- merge(pass.dat, hold)
rm(hold, i)

## Plotting ----------------------------------------------------------------
# Water Quality
WQplot <- function(var, type = 'B', system = 'all'){
  title.type <- switch(type,
                       B = 'Bottom',
                       S = 'Surface')
  title.print <- switch(var,
                  Depth = 'Depth (m)',
                  Temp = 'Temperature (°C)',
                  Cond = 'Conductivity (mS/cm)',
                  Sal = 'Salinity',
                  DO.pct = 'Dissolved Oxygen (% Saturation)',
                  DO.mg_l = 'Dissolved Oxygen (mg/L)',
                  pred.growth = 'Predicted Growth (per day)')
  title.print <- paste(title.type, title.print, sep = ' ')
  
  legend.print <- switch(var,
                   Depth = 'Depth',
                   Temp = 'Temperature',
                   Cond = 'Conductivity',
                   Sal = 'Salinity',
                   DO.pct = 'DO (%)',
                   DO.mg_l = 'DO (mg/L)',
                   pred.growth = 'Growth')
  
  system.plot <- switch(system,
                        all = yk.df,
                        YK = yk.plot,
                        PK = pk.plot)
  
  system.dat <- switch(system,
                   all = pass.dat,
                   YK = filter(pass.dat, grepl('YK', Site.ID)),
                   PK = filter(pass.dat, grepl('PK', Site.ID)))
  
  ggplot(environment = environment()) +
  geom_point(data = filter(system.dat, Type == type),
             aes(x = DD.Long, y = DD.Lat, color = DO.mg_l, size = Detections),
                 environment = environment())+ 
  facet_wrap(~ d.range) +
  scale_color_continuous(low = 'blue', high = 'orange') + 
  scale_size_manual(values = c(4,7,10,12), breaks = c('0','1','2','3')) +
  geom_point(data = filter(system.dat, Detections != '0'),
             aes(x = DD.Long, y = DD.Lat, size = Detections),
             shape = 21, color = 'black') +
  geom_polygon(data = system.plot,
                     aes(x = long, y = lat, group = group),
                     fill = 'lightgray', color = 'black', alpha = 0.3) +
  theme_bw() +
  labs(x = 'Longitude', y = 'Latitude', title = title.print,
       color = legend.print)
}

# png(file="p:/obrien/biotelemetry/csi/listening/2014 images/BotDOmgl.png",
#     width = 1200, height = 650, res = 90)
WQplot('pred.growth')
# dev.off()


# Only Detections, no water quality
# png(file="p:/obrien/biotelemetry/csi/listening/2014 images/alldetections.png",
#     width = 1200, height = 650, res = 90)
ggplot() +
  facet_wrap(~ d.range) +
  scale_size_manual(values = c(4,7,10,12), breaks = c('0','1','2','3')) +
  geom_polygon(data = yk.df,
                     aes(x = long, y = lat, group = group),
                     fill = 'lightgray', color = 'black') +
  geom_point(data = filter(pass.dat, Detections != '0'),
             aes(x = DD.Long, y = DD.Lat),
             color = 'red') +
  theme_bw() +
  labs(x = 'Longitude', y = 'Latitude', title = 'Atlantic Sturgeon Detections')
# dev.off()


## Other Functions
# Plot estimated receiver range. circ.filt = T drops circle points on land.
range.plot <- function(system, cruise, circ.filt = F){
  map <- if(grepl('p', system, ignore.case = T)){
    pk.plot
  } else{
    yk.plot
  }
  
  sys.title <- ifelse(grepl('p', system, ignore.case = T), 'Pamunkey', 'York')
  sys.title <- paste(sys.title, cruise, sep = ' ')
  
  system <- if(grepl('p', system, ignore.case = T)){
    filter(pass.dat, grepl('PK', Site.ID))
  } else{
    filter(pass.dat, grepl('YK', Site.ID))
  }
  cruise <- filter(system, Cruise == cruise)


  circ.9 <- TelemetryR::ptcirc(select(cruise, DD.Long, DD.Lat), 900)
  circ.5 <- TelemetryR::ptcirc(select(cruise, DD.Long, DD.Lat), 500)
  if(circ.filt == T){
    cirselect <- function(cirpts, map){
      pts <- SpatialPoints(cirpts[,c('long','lat')],
                           proj4string = map@proj4string)
      pts.over <- over(pts, map)
      # might need to change column to a different identifier
      # (just looking for NA, here)
      pts.bad <- rownames(pts.over[is.na(pts.over$ID),])
      cirpts[!(rownames(cirpts) %in% pts.bad), ]
    }
    circ.9 <- cirselect(circ.9, york)
    circ.5 <- cirselect(circ.5, york)
  }
  
  
  ggplot() + geom_path(data = map, aes(x = long, y = lat, group = group)) +
    geom_polygon(data = circ.9,
                 aes(long, lat, group = circle), fill = 'red', alpha = 0.2) +
    geom_polygon(data = circ.5,
                 aes(long, lat, group = circle), fill = 'green', alpha = 0.3) +
    geom_point(data = cruise, aes(x = DD.Long, y = DD.Lat, size = Detections)) +
    scale_size_manual(values = c(1,3,5,7), breaks = c('0','1','2','3')) +
    labs(x = 'Longitude', y = 'Latitude', title = sys.title) 
}

range.plot('yk', '2014_1')

# Cruise-specific water quality
env.plot <- function(system, cruise, env.var, type = 'B'){
  map <- if(grepl('p', system, ignore.case = T)){
    pk.plot
  } else{
    yk.plot
  }
  sys.title <- ifelse(grepl('p', system, ignore.case = T), 'Pamunkey', 'York')
  sys.title <- paste(sys.title, cruise, sep = ' ')
  
  system <- if(grepl('p', system, ignore.case = T)){
    filter(pass.dat, grepl('PK', Site.ID))
  } else{
    filter(pass.dat, grepl('YK', Site.ID))
  }
  
  cruise <- filter(system, Cruise == cruise, Type == type)
  
  plot.call <-
    substitute(ggplot() +
    geom_path(data = map, aes(x = long, y = lat, group = group)) +
    geom_point(data = cruise,
               aes(x = DD.Long, y = DD.Lat, color = VAR),
               size = 10) +
    scale_color_gradient(low = 'blue', high = 'pink') +
    geom_point(data = cruise,
               aes(x = DD.Long, y = DD.Lat, size = Detections)) +
    scale_size_manual(values = c(1,3,5,7), breaks = c('0','1','2','3')) +
    labs(x = 'Longitude', y = 'Latitude', title = sys.title),
    list(VAR = as.name(env.var)))
  eval(plot.call)
}
  
env.plot('pk', '2014_5', 'Temp','S')