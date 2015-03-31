library(rgdal); library(ggplot2); library(dplyr)

# Input -------------------------------------------------------------------
# Load shapefile. Note that for the readOGR function, capitalization in the
# first quoted string doesn't matter, but it does in the second string. The
# second string is the name of the shapefile. capitalization does matter here,
# but leave off the file extension.
ches <- readOGR('c:/users/secor lab/desktop/gis products/erin',
                'reporting_regions_new')

# Need to change projection from UTM to lat/long
ches <- spTransform(ches, CRS('+proj=longlat'))

# Rivers
levels(ches$FIRST_REP_)
# Subset shapefile by river
james <- ches[ches$FIRST_REP_ == 'James River',]

# Load Data
spr_grow <- read.csv('c:/users/secor lab/desktop/1990 Spring predicted growth CB_production averages.csv', stringsAsFactors = F)

# Pull out columns I need (used dplyr package here)
spr_grow <- spr_grow %>% 
  select(long, lat, gpred)

# Map manipulation ----------------------------------------------------------
# Make data into a spatial object. I kept the lat/long in the data slot of the
# spatial object so the info can be pulled together later. Make sure this is
# projected the same way as the shapefile.
spr_grow <- SpatialPointsDataFrame(coords = spr_grow[, 1:2], data = spr_grow,
                                   proj4string = CRS(proj4string(ches)))

# Pull out points that are within the James River section we subsetted earlier
spr_grow_james <- spr_grow[james,]

# There may be a few points that are left out. You can see them as blue points
# amoungst the red in the James.
plot(james)
points(spr_grow, col = 'blue')
points(spr_grow_james, col = 'red')

# Data Stuff ----------------------------------------------------------------
# You should be able to access things like a normal data frame.
# Note that the data is actually held in a "slot" accessed using "@data". For
# certain things (like the ggplot code below) you need to call up the data
# this way. Both of the lines below will produce the same thing, so you don't
# have to worry about this for most simple analyses.
summary(spr_grow_james@data$gpred)
summary(spr_grow_james$gpred)

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.01440 0.02775 0.04379 0.03975 0.05254 0.05531


# I use ggplot a lot, as it helps with quickly visualizing data
# Make the spatial object into a data frame (what ggplot recognizes)
james_fort <- fortify(james) 

# Plot stuff
ggplot() + geom_point(data = data.frame(spr_grow_james@data),
                      aes(x = long, y = lat, color = gpred), size = 4) +
  scale_color_gradient(low = 'red', high = 'blue') +
  geom_polygon(data = james_fort,
                     aes(x = long, y = lat, group = group),
                     fill = 'lightgrey', color = 'black', alpha = 0.3) +
  labs(x = 'Longitude', y = 'Latitude', color = 'Growth') +
  theme_bw()
