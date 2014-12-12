library(dplyr); library(ggplot2)
act.dat <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                     stringsAsFactors = F)
dist <- read.csv('distances.csv', header = F,
                 col.names = c('Site.ID', 'PKrivkm'))

pk.dat <- act.dat %>%
  left_join(dist) %>%
  mutate(PKrivkm = PKrivkm/1000,
         Date = lubridate::mdy(Date)) %>%
  filter(grepl('PK', Site.ID))

ggplot() + geom_point(data = filter(pk.dat, Detections > 0, Type == 'B'),
                      aes(x = Date, y = PKrivkm,
                          size = factor(Detections), color = Cond)) +
  scale_color_continuous(low = 'blue', high = 'orange') +
  scale_size_manual(values = c(4,7,10,12), breaks = c('0','1','2','3')) +
  ylim(c(0, 72)) +
  labs(x = 'Date (2014)', y = 'River Kilometer',
       title = 'Pamunkey River Atlantic Sturgeon Detections',
       size = 'Detections')