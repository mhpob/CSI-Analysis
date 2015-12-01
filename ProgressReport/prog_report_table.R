library(TelemetryR); library(dplyr)

all.det <- vemsort('p:/obrien/biotelemetry/detections')
ACT <- ACTtrans
all <- data.frame(all.det)

arr <- function(part){grepl(part, all[, 8], ignore.case = T)}
all$array <- ifelse(arr('cbl'), 'CBL Pier',
            ifelse(arr('cedar'), 'Cedar Point',
            ifelse(arr('piney'), 'Piney Point',
            ifelse(arr('301'), 'Rt 301',
            ifelse(arr('kent'), 'Kent Island',
            ifelse(arr('chop'), 'Choptank',
            ifelse(arr('marsh'), 'Marshyhope',
            ifelse(arr('nan|ferry|draw|broad|deep'), 'Nanticoke',
            ifelse(arr('poco'), 'Pocomoke',
            ifelse(arr('v-|t-|a-'), 'MD Coast',
                    'Other'))))))))))

ACT$Common.Name <- ifelse(grepl("Striped bass", ACT$Common.Name, ignore.case = T),
                          "Striped bass", ACT$Common.Name)

selected <- all %>%
  filter(date.local >= '2015-07-01') %>%
  left_join(select(ACT, Tag.ID.Code.Standard, Common.Name, Primary.Researcher),
            by = c('transmitter' = 'Tag.ID.Code.Standard')) %>%
  filter(Common.Name == 'Atlantic sturgeon') %>%
  mutate(dates = lubridate::floor_date(date.local, unit = 'week')) %>%
  group_by(Common.Name, dates, array, Primary.Researcher) %>%
  summarize(detections = n())


write.csv(selected, 'selected.csv')
