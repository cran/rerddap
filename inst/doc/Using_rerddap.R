## ----initialize, echo = FALSE--------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>", 
  cache.path = "cache/",
  fig.path = "fig/"
)
library(rerddap)
data("colors")

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("rerddap")

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("ropensci/rerddap")

## ------------------------------------------------------------------------
library("rerddap")

## ---- echo = FALSE, warning = FALSE, warn.conflicts = FALSE, message = FALSE----
library("akima")
library("dplyr")
library("ggplot2")
library("mapdata")
library("ncdf4")
library("plot3D")

## ---- eval = FALSE-------------------------------------------------------
#  library("akima")
#  library("dplyr")
#  library("ggplot2")
#  library("mapdata")
#  library("ncdf4")
#  library("plot3D")

## ---- eval = FALSE-------------------------------------------------------
#  ?rerddap

## ---- eval = FALSE-------------------------------------------------------
#  whichSST <- ed_search(query = "SST")

## ---- eval = FALSE-------------------------------------------------------
#  whichSST <- ed_search(query = "SST MODIS")

## ------------------------------------------------------------------------
servers()

## ------------------------------------------------------------------------
100*100*4*8*365

## ---- eval = FALSE-------------------------------------------------------
#  browse('jplMURSST41')
#  browse('siocalcofiHydroCasts')

## ------------------------------------------------------------------------
info('jplMURSST41')
info('siocalcofiHydroCasts')

## ---- eval = FALSE-------------------------------------------------------
#  latitude = c(22., 51.)
#  longitude = c(-140., -105)
#  time = c("2017-01-01", "2017-01-02")

## ---- eval = FALSE-------------------------------------------------------
#  sstInfo <- info('jplMURSST41')
#  murSST <- griddap(sstInfo, latitude = c(22., 51.), longitude = c(-140., -105), time = c("2017-01-01", "2017-01-02"), fields = 'analysed_sst')
#  

## ---- eval = FALSE-------------------------------------------------------
#  murSST <- griddap(sstInfo, latitude = c(22., 51.), longitude = c(-140., -105), time = c("2017-01-01", "2017-01-02"), stride = c(1,1,5), fields = 'analysed_sst')
#  

## ---- eval = FALSE-------------------------------------------------------
#  'time>=2010-01-01'
#  'time<=2010-12-31'
#  'scientific_name="Sardinops sagax"'
#  

## ----VIIRS, fig.width = 6, fig.height = 3, fig.align = 'center', warning = FALSE----
require("ggplot2")
require("mapdata")
require("rerddap")
sstInfo <- info('erdVHsstaWS3day')
# get latest 3-day composite sst
viirsSST <- griddap(sstInfo, latitude = c(41., 31.), longitude = c(-128., -115), time = c('last','last'), fields = 'sst')
# remap latitiudes and longitudes to even grid
myLats <- unique(viirsSST$data$lat)
myLons <- unique(viirsSST$data$lon)
myLats <- seq(range(myLats)[1], range(myLats)[2], length.out = length(myLats))
myLons <- seq(range(myLons)[1], range(myLons)[2], length.out = length(myLons))
# melt these out to full grid
mapFrame <- expand.grid(x = myLons, y = myLats)
mapFrame$y <- rev(mapFrame$y)
# form a frame with the new values and the data
tempFrame <- data.frame(sst = viirsSST$data$sst, lat = mapFrame$y, lon = mapFrame$x)
mycolor <- colors$temperature
w <- map_data("worldHires", ylim = c(30., 42.), xlim = c(-128, -114))
ggplot(data = tempFrame, aes(x = lon, y = lat, fill = sst)) +
    geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradientn(colours = mycolor, na.value = NA) +
    theme_bw() + ylab("latitude") + xlab("longitude") +
    coord_fixed(1.3, xlim = c(-128, -114),  ylim = c(30., 42.)) + ggtitle("Latest VIIRS 3-day SST")


## ------------------------------------------------------------------------
ggplot(tempFrame, aes(time, sst)) + 
  geom_line() + 
  theme_bw() + 
  ylab("sst") +
  ggtitle("VIIRS SST at (36N, 126W)")

## ------------------------------------------------------------------------
# get latest 3-day composite sst
mycolor <- colors$chlorophyll
w <- map_data("worldHires", ylim = c(30., 42.), xlim = c(-128, -114))
ggplot(data = viirsCHLA$data, aes(x = lon, y = lat, fill = log(chla))) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-128, -114),  ylim = c(30., 42.)) + 
  ggtitle("Latest VIIRS 3-day Chla")

## ----soda70Plot, fig.width = 6, fig.height = 3, fig.align = 'center', warning = FALSE----
require("ggplot2")
require("mapdata")
xlim <- c(135, 240)
ylim <- c(20, 60)
my.col <- colors$temperature
## Must do a kludge to remove countries that wrap and mess up the plot
w1 <- map("world2Hires", xlim = c(135, 240), ylim = c(20, 60), fill = TRUE, plot = FALSE)
remove <- c("UK:Great Britain", "France", "Spain", "Algeria", "Mali", "Burkina Faso", "Ghana", "Togo")
w <- map_data("world2Hires", regions = w1$names[!(w1$names %in% remove)], ylim = ylim, xlim = xlim)
myplot <- ggplot() +
    geom_raster(data = soda70$data, aes(x = lon, y = lat, fill = temp), interpolate = FALSE) +
    geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
    theme_bw() + scale_fill_gradientn(colours = my.col, na.value = NA, limits = c(-3,30), name = "temperature") +
    ylab("latitude") + xlab("longitude") +
    coord_fixed(1.3, xlim = xlim, ylim = ylim) +
    ggtitle(paste("70m temperature ", soda70$data$time[1]))
myplot

## ----NAtlSSSplot, fig.width = 6, fig.height = 3, fig.align = 'center', warning = FALSE----
require("ggplot2")
require("mapdata")
xlim <- c(-17.99375, -1.00625)
ylim <- c(48.00625, 57.50625)
my.col <- colors$salinity
w <- map_data("worldHires", ylim = ylim, xlim = xlim)
myplot <- ggplot() +
    geom_raster(data = NAtlSSS$data, aes(x = lon, y = lat, fill = sea_surface_salinity), interpolate = FALSE) +
    geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
    theme_bw() + scale_fill_gradientn(colours = my.col, na.value = NA, limits = c(34, 36), name = "salinity") +
    ylab("latitude") + xlab("longitude") +
    coord_fixed(1.3, xlim = xlim, ylim = ylim) +
    ggtitle(paste("salinity", NAtlSSS$data$time[1]))
myplot

## ----ifrPSALplot, fig.width = 6, fig.height = 3, fig.align='center', warning = FALSE----
## ggplot2 has trouble with unequal y's
 require("akima")
 require("dplyr")
 require("ggplot2")
 require("mapdata")
  xlim <- c(-140, -110)
  ylim <- c(30, 51)
## ggplot2 has trouble with unequal y's
  my.col <- colors$salinity
  tempData1 <- ifrPSAL$data$PSAL
  tempData <- array(tempData1 , 61 * 54)
  tempFrame <- data.frame(x = ifrPSAL$data$lon, y = ifrPSAL$data$lat)
  tempFrame$temp <- tempData
  tempFrame1 <- dplyr::filter(tempFrame, !is.nan(temp))
  myinterp <- akima::interp(tempFrame1$x, tempFrame1$y, tempFrame1$temp, xo = seq(min(tempFrame1$x), max(tempFrame1$x), length = 61), yo = seq(min(tempFrame1$y), max(tempFrame1$y), length = 54))
  myinterp1 <- expand.grid(x = myinterp$x, y = myinterp$y)
  myinterp1$temp <- array(myinterp$z, 61 * 54)
  w <- map_data("worldHires", ylim = ylim, xlim = xlim)
 myplot <- ggplot() +
    geom_raster(data = myinterp1, aes(x = x, y = y, fill = temp), interpolate = FALSE) +
    geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
    theme_bw() + scale_fill_gradientn(colours = my.col, na.value = NA, limits = c(32, 35), name = "salinity") +
    ylab("latitude") + xlab("longitude") +
    coord_fixed(1.3, xlim = xlim, ylim = ylim) + ggtitle(paste("salinity at 75 meters",ifrPSAL$data$time[1] ))
 myplot

## ------------------------------------------------------------------------
require("rerddap")
hydroInfo <- info('siocalcofiHydroCasts')

## ----calCOFInum----------------------------------------------------------
calcofi.df$cruz_num <- as.numeric(calcofi.df$cruz_num)
calcofi.df$intc14 <- as.numeric(calcofi.df$intc14)
calcofi.df$time <- as.Date(calcofi.df$time, origin = '1970-01-01', tz = "GMT")

## ----calCOFIPlot, fig.width = 6, fig.height = 3, fig.align='center', fig.show = 'hold', warning = FALSE----
require("dplyr")

# calculate cruise means
by_cruznum <- group_by(calcofi.df, cruz_num)
tempData <- select(by_cruznum, year, month, cruz_num, intchl, intc14)
CruiseMeans <- summarize(by_cruznum, cruisechl = mean(intchl, na.rm = TRUE), cruisepp = mean(intc14, na.rm = TRUE), year = median(year, na.rm = TRUE), month = median(month, na.rm = TRUE))
tempTimes <- paste0(CruiseMeans$year,'-',CruiseMeans$month,'-1')
cruisetimes <- as.Date(tempTimes, origin = '1970-01-01', tz = "GMT")
CruiseMeans$cruisetimes <- cruisetimes
# calculate monthly "climatologies"
byMonth <- group_by(CruiseMeans, month)
climate <- summarize(byMonth, ppClimate = mean(cruisepp, na.rm = TRUE), chlaClimate = mean(cruisechl, na.rm = TRUE))
# calculate anomalies
CruiseMeans$chlanom <- CruiseMeans$cruisechl - climate$chlaClimate[CruiseMeans$month]
CruiseMeans$ppanom <- CruiseMeans$cruisepp - climate$ppClimate[CruiseMeans$month]
# calculate mean yearly anomaly
byYear <- select(CruiseMeans, year)
tempData <- select(CruiseMeans, year, chlanom, ppanom )
byYear <- group_by(tempData, year)
yearlyAnom <- summarize(byYear, ppYrAnom = mean(ppanom, na.rm = TRUE), chlYrAnom = mean(chlanom, na.rm = TRUE))
yearlyAnom$year <- ISOdate(yearlyAnom$year, 01, 01, hour = 0)
ggplot(yearlyAnom, aes(year, chlYrAnom)) + geom_line() +
  theme_bw() + ggtitle('yearly chla anom')
ggplot(yearlyAnom, aes(year, ppYrAnom)) + geom_line() +
  theme_bw() + ggtitle('yearly pp anom')

## ----CPSINfo-------------------------------------------------------------
require("rerddap")
(CPSinfo <- info('FRDCPSTrawlLHHaulCatch'))

## ----CPSPlot, fig.width = 6, fig.height = 6, fig.align='center', fig.show = 'hold', warning = FALSE----
# get polygons of coast for this area
w <- map_data("worldHires", ylim = c(22., 51.), xlim = c(220 - 360, 250 - 360))
# plot 201004 sst on the map
sardine2010 <- filter(sardines, time < as.Date('2010-12-01', origin = '1970-01-01', tz = "GMT"))
sardine2011 <- filter(sardines, time > as.Date('2010-12-01', origin = '1970-01-01', tz = "GMT"))
mycolor <- colors$temperature
p1 <- ggplot() +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(data = sst201004$data, aes(x = lon - 360, y = lat, fill = sst), interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA, limits = c(5,30)) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(220 - 360, 250 - 360),  ylim = c(22., 51.))

# plot 201104 sst on the map
p2 <- ggplot() +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(data = sst201104$data, aes(x = lon - 360, y = lat, fill = sst), interpolate = FALSE) +
  geom_point(data = sardine2011, aes(x = longitude, y = latitude, colour = subsample_count)) +
  scale_fill_gradientn(colours = mycolor, na.value = NA, limits = c(5,30)) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(220 - 360, 250 - 360),  ylim = c(22., 51.))
p1 + geom_point(data = sardine2010, aes(x = longitude, y = latitude, colour = subsample_count)) + scale_colour_gradient(space = "Lab", na.value = NA, limits = c(0,80))

p2 +   geom_point(data = sardine2011, aes(x = longitude, y = latitude, colour = subsample_count)) + scale_colour_gradient(space = "Lab", na.value = NA, limits = c(0,80))

## ----sardinesPlot, fig.width = 6, fig.height = 5, fig.align='center', warning = FALSE----
xlim <- c(-135, -110)
ylim <- c(30, 51)
coast <- map_data("worldHires", ylim = ylim, xlim = xlim)
ggplot() +
    geom_point(data = sardinops, aes(x = longitude, y = latitude, colour = year)) +
    geom_polygon(data = coast, aes(x = long, y = lat, group = group), fill = "grey80") +
    theme_bw() + ylab("latitude") + xlab("longitude") +
    coord_fixed(1.3, xlim = xlim, ylim = ylim) +
    ggtitle("Location of sardines by year in EPM Trawls")

## ----NDBC, fig.width = 6, fig.height = 5, fig.align='center', warning = FALSE----
xlim <- c(-130, -110)
ylim <- c(35, 50)
coast <- map_data("worldHires", ylim = ylim, xlim = xlim)
ggplot() +
   geom_point(data = locationBuoys, aes(x = longitude , y = latitude, colour = factor(station) )) +
   geom_polygon(data = coast, aes(x = long, y = lat, group = group), fill = "grey80") +
   theme_bw() + ylab("latitude") + xlab("longitude") +
   coord_fixed(1.3, xlim = xlim, ylim = ylim) +
   ggtitle("Location of buoys in given region")


## ----NDBCTS, fig.width = 6, fig.height = 3, fig.align='center', warning = FALSE----
ggplot(buoyData, aes(time, wspd)) + 
  geom_line() + 
  theme_bw() + 
  ylab("wind speed") +
  ggtitle("Wind Speed in 2012 from buoy 46012 ")

## ----glider, fig.width = 5, fig.height = 5, fig.align = 'center', warning = FALSE----
require("plot3D")
scatter3D(x = glider$longitude , y = glider$latitude , z = -glider$depth, colvar = glider$salinity,              col = colors$salinity, phi = 40, theta = 25, bty = "g", type = "p",
           ticktype = "detailed", pch = 10, clim = c(33.2,34.31), clab = 'Salinity',
           xlab = "longitude", ylab = "latitude", zlab = "depth",
           cex = c(0.5, 1, 1.5))

## ---- fig.width = 6, fig.height = 3, fig.align = 'center', warning = FALSE----
ylim <- c(32.5, 34)
xlim <- c(-119, -116.5)
mycolor <- colors$temperature
w <- map_data("worldHires", ylim = ylim, xlim = xlim)
alldata <- data.frame(sst = ncdcSST, longitude = atnData$longitude - 360, latitude = atnData$latitude)
z <- ggplot(alldata, aes(x = longitude, y = latitude)) +
   geom_point(aes(colour = sst), size = .5)
z + geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  theme_bw() +
  scale_colour_gradientn(colours = mycolor, limits = c(16.9, 17.3), "SST") +
  coord_fixed(1.3, xlim = xlim, ylim = ylim) + ggtitle("SST Along Track")


## ---- fig.width = 6, fig.height = 3, fig.align = 'center', warning = FALSE----
ggplot(alldata) + geom_line(aes(x = year, y = coho), colour = 'blue') + theme_bw() + ggtitle("Log(coho)")
ggplot(alldata) + geom_line(aes(x = year, y = maxSLP), colour = 'red') + theme_bw() + ggtitle("MaxSLP")

## ----  eval = FALSE------------------------------------------------------
#  ?cache_delete
#  ?cache_delete_all
#  ?cache_details
#  ?cache_list

## ------------------------------------------------------------------------
require("ncdf4")
exampleFile <- system.file("extdata", "MWsstd1day.nc", package = "rerddap")
sstFile <- nc_open(exampleFile)

## ------------------------------------------------------------------------
names(sstFile$dim)

## ------------------------------------------------------------------------
sstFile$dim$longitude$vals

## ------------------------------------------------------------------------
names(sstFile$var)

## ------------------------------------------------------------------------
require("ncdf4")
day1SST <- ncvar_get(sstFile, "sst", start = c(1, 1, 1, 1), count = c(1, 1, -1, -1))

## ------------------------------------------------------------------------
latMin <- which(sstFile$dim$latitude$vals == 30.0)
latMax <- which(sstFile$dim$latitude$vals == 30.5)
lonMin <- which(sstFile$dim$longitude$vals == 210.0)
lonMax <- which(sstFile$dim$longitude$vals == 210.5)

## ------------------------------------------------------------------------
require("ncdf4")
day1SST <- ncvar_get(sstFile, "sst", start = c(lonMin, latMin, 1, 1), count = c( (lonMax - lonMin + 1), (latMax - latMin + 1), 1, 1 ))

## ------------------------------------------------------------------------
require("ncdf4")
day1SST <- ncvar_get(sstFile, "sst", start = c(lonMax, latMin, 1, 1), count = c(1, 1, 1, -1 ))

## ------------------------------------------------------------------------
latMin <- max(which(sstFile$dim$latitude$vals <= 30.1))
latMax <- min(which(sstFile$dim$latitude$vals >= 30.3))
lonMin <- max(which(sstFile$dim$longitude$vals <= 210.1))
lonMax <- min(which(sstFile$dim$longitude$vals >= 210.3))


## ------------------------------------------------------------------------
require("ncdf4")
day1SST <- ncvar_get(sstFile, "sst", start = c(lonMin, latMin, 1, 1), count = c( (lonMax - lonMin + 1), (latMax - latMin + 1), 1, 1 ))

