library(dplyr); library(ggplot2)
act.dat <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                     stringsAsFactors = F)
act.dat <- act.dat %>% 
  mutate(pred.growth = TelemetryR::sturgrow(Temp, Sal, DO.pct),
         river = substr(Site.ID, 1, 2))
dist <- read.csv('distances.csv', header = F, stringsAsFactors = F,
                 col.names = c('Site.ID', 'PKrivkm'))

hold <- data.frame(Cruise = levels(factor(act.dat$Cruise)),
                   d.range = NA)
for(i in seq(1, dim(hold)[1],1)){
  hold$d.range[i] <- paste(range(filter(act.dat, Cruise == hold[i, 1])$Date),
                        collapse = ' - ')
}
hold$d.range <- factor(hold$d.range, levels = hold$d.range, ordered = T)
act.dat <- merge(act.dat, hold)
rm(hold, i)

pk.dat <- act.dat %>%
  left_join(dist) %>%
  mutate(PKrivkm = PKrivkm/1000,
         Date = lubridate::mdy(Date),
         altdepth = 19 - Depth,
         year = lubridate::year(Date)) %>%
  filter(grepl('PK', Site.ID))



## Date v river kilometer plotting #############################################
temp <-
  ggplot() + geom_point(data = filter(pk.dat, Detections > 0, Type == 'B'),
                        aes(x = lubridate::yday(Date), y = PKrivkm,
                            size = factor(Detections), color = Temp)) +
  scale_color_continuous(low = 'blue', high = 'orange') +
  scale_size_manual(values = c(4,7,10,12), breaks = c('0','1','2','3')) +
  facet_wrap(~year, ncol = 1) +
  ylim(c(0, 72)) +
  labs(x = 'Day of Year', y = 'River Kilometer',
       size = 'Detections', color = 'Temp. (°C)')

sal <-
  ggplot() + geom_point(data = filter(pk.dat, Detections > 0, Type == 'B'),
                        aes(x = lubridate::yday(Date), y = PKrivkm,
                            size = factor(Detections), color = Sal)) +
  scale_color_continuous(low = 'blue', high = 'orange') +
  scale_size_manual(values = c(4,7,10,12), breaks = c('0','1','2','3')) +
  facet_wrap(~year, ncol = 1) +
  ylim(c(0, 72)) +
  labs(x = 'Day of Year', y = 'River Kilometer',
       size = 'Detections', color = 'Salinity')

dopct <-
  ggplot() + geom_point(data = filter(pk.dat, Detections > 0, Type == 'B'),
                        aes(x = lubridate::yday(Date), y = PKrivkm,
                            size = factor(Detections), color = DO.pct)) +
  scale_color_continuous(low = 'blue', high = 'orange') +
  scale_size_manual(values = c(4,7,10,12), breaks = c('0','1','2','3')) +
  facet_wrap(~year, ncol = 1) +
  ylim(c(0, 72)) +
  labs(x = 'Day of Year', y = 'River Kilometer',
       size = 'Detections', color = 'D.O. (%)')

predgr <-
  ggplot() + geom_point(data = filter(pk.dat, Detections > 0, Type == 'B'),
                        aes(x = lubridate::yday(Date), y = PKrivkm,
                            size = factor(Detections), color = pred.growth)) +
  scale_color_continuous(low = 'blue', high = 'orange') +
  scale_size_manual(values = c(4,7,10,12), breaks = c('0','1','2','3')) +
  facet_wrap(~year, ncol = 1) +
  ylim(c(0, 72)) +
  labs(x = 'Day of Year', y = 'River Kilometer',
       size = 'Detections', color = 'Growth')

library(gridExtra)
png('p:/obrien/biotelemetry/csi/listening/2014 images/datevkm2.png',
    height = 650, width = 1200)
grid.arrange(grobs = list(sal, dopct, temp, predgr),
             main = 'Pamunkey River Atlantic Sturgeon Detections')
dev.off()

## River kilometer vs Depth vs Water Quality ###################################
png('p:/obrien/biotelemetry/csi/listening/2014 images/kmvdepthvpredgrowth.png',
    height = 775, width = 1200)
ggplot() + geom_rect(data = pk.dat,
            aes(xmin = 1.5, xmax = 71.25,
                ymin = 0, ymax = 20),
            fill = 'gray') +
  geom_area(data = filter(pk.dat, Type == 'B'),
                     aes(x = PKrivkm, y = Depth),
            fill = 'white', color = 'black') + 
  geom_rect(data = filter(pk.dat, Type == 'B', 
                          Detections > 0),
            aes(xmin = PKrivkm - 0.8, xmax = PKrivkm + 0.8,
                ymin = -0.25, ymax = 20),
            color = 'black', fill = NA) +
  geom_point(data = pk.dat,
             aes(x = PKrivkm, y = Depth, color = pred.growth), size = 4) +
  facet_wrap(~d.range, ncol = 1) +
  scale_x_reverse() + scale_y_reverse() +
  scale_color_continuous(low = 'blue', high = 'orange') +
  labs(x = 'Pamunkey River Kilometer', y = 'Depth (m)', color = 'Growth') +
  theme_bw()
dev.off()