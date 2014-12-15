library(TelemetryR); library(ggplot2); library(dplyr)
listen.dat <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                     header = T, stringsAsFactors = F)

listen.dat <- listen.dat %>% 
  mutate(pred.growth = sturgrow(Temp, Sal, DO.pct),
         river = substr(Site.ID, 1, 2))

ld_bot <- listen.dat %>%
  filter(Type == 'B')

ld_mean <- listen.dat %>%
  group_by(Site.ID, Cruise) %>%
  summarize(temp = mean(Temp),
            do = mean(DO.pct),
            sal = mean(Sal),
            det = mean(Detections))

# Plot general histogram, histogram where detections > 0, cumdist of detections
hist_cum_plot <- function (var, width) {
  brks <- seq(floor(range(ld_bot[, var])[1]), ceiling(range(ld_bot[, var])[2]),
              width)
  
  cum.dat <- substitute(ld_bot %>% filter(Detections > 0) %>% arrange(var) %>%
                           mutate(cumulative = cumsum(Detections),
                                  cumulative = cumulative/max(cumulative)*100),
                        list(var = as.name(var)))
  cum.dat <- eval(cum.dat)
  
  
  par(mar = c(5, 4, 4, 5) + 0.1)
  j <- hist(ld_bot[, var], breaks = brks, plot = F)
  hist(ld_bot[, var], breaks = brks,
       xlim = c(floor(range(ld_bot[, var])[1]),
                ceiling(range(ld_bot[, var])[2])),
       ylim = c(0, max(j$counts)),
       xlab = var,
       main = paste('Histogram of', var))
  
  par(new = T)
  hist(ld_bot[ld_bot$Detections > 0, var], breaks = brks,
       col = 'red', xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = '',
       xlim = c(floor(range(ld_bot[, var])[1]),
                ceiling(range(ld_bot[, var])[2])),
       ylim = c(0, max(j$counts)))
  
  par(new = T)
  plot(cum.dat[, var], cum.dat[, 19], 
       xlim = c(floor(range(ld_bot[, var])[1]),
                ceiling(range(ld_bot[, var])[2])),
       type = 'l', col = 'blue',
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = '',)
  axis(4, at = c(0, 20, 40, 60, 80, 100), col.axis = 'blue')
    mtext('Cumulative Detections (%)', side = 4, line = 3, col = 'blue')
}

hist_cum_plot('Temp', 2)
hist_cum_plot('DO.pct', 5)
hist_cum_plot('DO.mg_l', 0.5)
hist_cum_plot('Sal', 2)
hist_cum_plot('Cond', 2)
hist_cum_plot('Depth', 2)

# Plot histogram and overlay detections via points
pts_histo <- function(var, brk){
  ld_bot <- listen.dat %>%
    filter(Type == 'B')
  ld_bot <- ld_bot %>%  
    mutate(bin = findInterval(ld_bot[, var],
                              seq(floor(range(ld_bot[, var])[1]),
                                  ceiling(range(ld_bot[, var])[2]), brk)))
  det.bins <- ld_bot %>%
    group_by(bin) %>%
    summarize(detect = sum(Detections))
  
  histo <- hist(ld_bot[, var], breaks = seq(floor(range(ld_bot[, var])[1]),
                                            ceiling(range(ld_bot[, var])[2]),
                                            brk),
                plot = F)
  mids <- data.frame(mids = histo$mids)
  mids$bin <- row.names(mids)
    
  det.bins <- merge(det.bins, mids)
  det.bins <- det.bins[det.bins$detect != 0,]

  par(mar = c(5, 4, 4, 5) + 0.1)
  plot(histo, main = var, xlab = var, 
       xlim = c(floor(range(ld_bot[, var])[1]),
                ceiling(range(ld_bot[, var])[2])))
  par(new = T)
  plot(det.bins$mids, det.bins$detect,
       xlim = c(floor(range(ld_bot[, var])[1]),
                ceiling(range(ld_bot[, var])[2])),
       col = 'blue', xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  axis(4, at = seq(0, ceiling(range(det.bins$detect)[2]), 5),
       col.axis = 'blue')
  mtext('Detections', side = 4, line = 3, col = 'blue')
}

pts_histo('Temp', 1)
pts_histo('DO.pct', 5)
pts_histo('Sal', 2)
pts_histo('Cond', 2)
