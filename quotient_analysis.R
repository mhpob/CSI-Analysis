library(TelemetryR); library(dplyr)

data <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                 stringsAsFactors = F)

data <- filter(data, Type == 'B') %>% 
  mutate(pred.growth = TelemetryR::sturgrow(Temp, Sal, DO.pct))

QAplot <- function(year, env, width){
  data.sub <- data[grepl(year, data$Cruise), env]
  min.data <- min(data[grepl(year, data$Cruise), env])
  max.data <- max(data[grepl(year, data$Cruise), env])
  
  hold <- quo_an(data.sub,
                 data[grepl(year, data$Cruise), 'Detections'],
                 bin_width = width, pres_abs = T)
  
  lims <- NULL
  lims[1] <- ifelse(abs(min.data) < 1,
                    min.data - abs(min.data) / 5,
                    floor(min.data))
  lims[2] <- ifelse(abs(max.data) < 1,
                    max.data + abs(min.data) / 5,
                    ceiling(max.data))
  brks <- seq(lims[1], lims[2], width)
  brks <- if (max.data > max(brks)) c(brks, max(brks) + width) else brks
  
  par(mar = c(4, 4, 4, 4) + 0.1) 
  hold.hist <- hist(data.sub, breaks = brks, freq = T,
                    xaxt = 'n', xlab = '', ylab = '', main = '')
  mtext('Number of Observations', side = 2, line = 2.25)
  
  par(new = T)
  plot(hold.hist$mids, hold$Qe, type = 'b', col = 'blue', lwd = 3,
       xlim = c(brks[1],brks[length(brks)]),
       yaxt = 'n', ylab = '', xlab = '', main = paste(year, env, sep = ', '))
  mtext('Value', side = 1, line = 2.25)
  axis(4, col.axis = 'blue', col = 'blue')
  mtext('Quotient', side = 4, line = 2.25, col = 'blue')
  
  abline(h = 1, lty = 3)
}

QAplot('2014', 'pred.growth', 0.005)
QAplot('2015', 'pred.growth', 0.005)
