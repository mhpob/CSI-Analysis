library(ggplot2); library(reshape2)

times <- read.csv(
  'p:/obrien/biotelemetry/csi/progress reports/2015/rec_deployeddates.csv')
times$d.st <- lubridate::ymd(times$d.st)
times$d.end <- lubridate::ymd(times$d.end)

tend <- read.csv(
  'p:/obrien/biotelemetry/csi/progress reports/2015/rec_tenddates.csv')
tend <- melt(tend, id = c('Station'), value.name = 'd.t', na.rm = T)
tend$d.t <- lubridate::ymd(tend$d.t)
tend <- merge(tend[,c(1,3)], times[,c(1,3)])
tend <- unique(tend)


j <- list(NULL)
for(i in 1:dim(times)[1]){
  k <- data.frame(Station = times[i, 'Station'],
                  order = times[i, 'ord'],
                  date = seq(times[i, 'd.st'], times[i, 'd.end'], by = 'day'))
  j[[i]] <- k
}
times <- do.call(rbind.data.frame, j)


labs <- c('C&D Canal', 'Kent Island A', 'Kent Island B', 'Kent Island C',
          'Kent Island D', 'CBL Pier', 'Cedar Point A', 'Cedar Point B',
          'Cedar Point D', 'Cedar Point E', 'Piney Point A', 'Piney Point B',
          'Piney Point C', 'Piney Point D', 'Rt 301 A', 'Rt 301 B', 'V-1',
          'V-2', 'V-3', 'T-1C', 'A-5C', 'T-2C', 'T-3C')



ggplot() + geom_raster(data = times,
                       aes(x = date, y = as.factor(order)),
                       fill = 'darkgreen') +
  geom_segment(data = tend,
               aes(x = d.t, y = ord - 0.45,
                   xend = d.t, yend = ord + 0.45),
               size = 2) +
  xlim(lubridate::ymd('2015-01-06'), lubridate::ymd('2015-06-25')) +
  scale_y_discrete(labels = rev(labs)) +
  theme_bw() +
  labs(x = 'Date', y = 'Station')
