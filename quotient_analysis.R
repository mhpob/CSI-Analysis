library(TelemetryR); library(dplyr)

data <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                 stringsAsFactors = F)

data <- filter(data, Type == 'B') %>%
  mutate(pred.growth = TelemetryR::sturgrow(Temp, Sal, DO.pct))

QAplot <- function(year, env, width){
  data.sub <- data[grepl(year, data$Cruise), env]
  min.data <- min(data.sub, na.rm = T)
  max.data <- max(data.sub, na.rm = T)

  QA.result <- quo_an(data.sub,
                       data[grepl(year, data$Cruise), 'Detections'],
                       bin_width = width, pres_abs = T, R = 2000)

  brks <- seq(min.data, max.data, width)
  brks <- if (max.data > max(brks)) c(brks, max(brks) + width) else brks
  lims <- (max.data - min.data) / length(brks)

  par(mar = c(4, 4, 4, 4) + 0.1)
  env.hist <- hist(data.sub, breaks = brks, freq = T,
                   xlim = c(min.data - lims, max.data + lims),
                   ylim = c(0, max(QA.result$wq.var, na.rm = T) + 10),
                   xaxt = 'n', xlab = '', ylab = '', yaxt = 'n', main = '')
  axis(4)
  mtext('Number of Observations', side = 4, line = 2.25)

  par(new = T)
  plot(env.hist$mids, QA.result$Qe, type = 'b', col = 'blue', lwd = 3,
       xlim = c(min.data - lims, max.data + lims),
       ylim = c(0, ceiling(max(QA.result[, 6:8], na.rm = T))),
       yaxt = 'n', ylab = '', xlab = '', main = paste(year, env, sep = ', '))
  lines(env.hist$mids, QA.result$CI_0.975, lty = 3, lwd = 2, col = 'lightblue')
  lines(env.hist$mids, QA.result$CI_0.025, lty = 3, lwd = 2, col = 'lightblue')
  mtext('Value', side = 1, line = 2.25)
  axis(2, col.axis = 'blue', col = 'blue')
  mtext('Quotient', side = 2, line = 2.25, col = 'blue')

  abline(h = 1, lty = 3, col = 'blue')
}

QAplot('2014', 'Temp', 2)
QAplot('2015', 'Temp', 2)
