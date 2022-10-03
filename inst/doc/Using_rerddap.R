## ----initialize, echo = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>", 
  cache.path = "cache/",
  fig.path = "man/figures/"
)
library(rerddap)
data("colors")

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("rerddap")

## ---- eval = FALSE------------------------------------------------------------
#  remotes::install_github("ropensci/rerddap")

## -----------------------------------------------------------------------------
library("rerddap")

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  library("akima")
#  library("dplyr")
#  library("ggplot2")
#  library("mapdata")
#  library("ncdf4")
#  library("plot3D")

## ---- eval = FALSE------------------------------------------------------------
#  ?rerddap

## ---- eval = FALSE------------------------------------------------------------
#  whichSST <- ed_search(query = "SST")

## ---- eval = FALSE------------------------------------------------------------
#  whichSST <- ed_search(query = "SST MODIS")

## -----------------------------------------------------------------------------
servers()

## -----------------------------------------------------------------------------
100*100*4*8*365

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  browse('jplMURSST41')
#  browse('siocalcofiHydroCast')

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  info('jplMURSST41')
#  info('siocalcofiHydroCast')

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  latitude = c(22., 51.)
#  longitude = c(-140., -105)
#  time = c("2017-01-01", "2017-01-02")

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  sstInfo <- info('jplMURSST41')
#  murSST <- griddap(sstInfo, latitude = c(22., 51.), longitude = c(-140., -105), time = c("2017-01-01", "2017-01-02"), fields = 'analysed_sst')
#  

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  murSST <- griddap(sstInfo, latitude = c(22., 51.), longitude = c(-140., -105), time = c("2017-01-01", "2017-01-02"), stride = c(1,1,5), fields = 'analysed_sst')
#  

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  'time>=2010-01-01'
#  'time<=2010-12-31'
#  'scientific_name="Sardinops sagax"'
#  

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  CPSinfo <- info('FRDCPSTrawlLHHaulCatch')
#  sardines <- tabledap(CPSinfo, fields = c('latitude',  'longitude', 'time', 'scientific_name', 'subsample_count'), 'time>=2010-01-01', 'time<=2010-12-31', 'scientific_name="Sardinops sagax"' )
#  

## ----MUR, eval = FALSE, echo = TRUE-------------------------------------------
#  require("ggplot2")
#  require("mapdata")
#  require("rerddap")
#  sstInfo <- info('jplMURSST41')
#  # get latest daily sst
#  murSST <- griddap(sstInfo, latitude = c(22., 51.), longitude = c(-140., -105), time = c('last','last'), fields = 'analysed_sst')
#  mycolor <- colors$temperature
#  w <- map_data("worldHires", ylim = c(22., 51.), xlim = c(-140, -105))
#  ggplot(data = murSST$data, aes(x = longitude, y = latitude, fill = analysed_sst)) +
#      geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#      geom_raster(interpolate = FALSE) +
#      scale_fill_gradientn(colours = mycolor, na.value = NA) +
#      theme_bw() + ylab("latitude") + xlab("longitude") +
#      coord_fixed(1.3, xlim = c(-140, -105),  ylim = c(22., 51.)) + ggtitle("Latest MUR SST")

## ----VIIRS, echo = TRUE, eval = FALSE-----------------------------------------
#  require("ggplot2")
#  require("mapdata")
#  require("rerddap")
#  sstInfo <- info('erdVHsstaWS3day')
#  # get latest 3-day composite sst
#  viirsSST <- griddap(sstInfo, latitude = c(41., 31.), longitude = c(-128., -115), time = c('last','last'), fields = 'sst')
#  # remap latitiudes and longitudes to even grid
#  myLats <- unique(viirsSST$data$latitude)
#  myLons <- unique(viirsSST$data$longitude)
#  myLats <- seq(range(myLats)[1], range(myLats)[2], length.out = length(myLats))
#  myLons <- seq(range(myLons)[1], range(myLons)[2], length.out = length(myLons))
#  # melt these out to full grid
#  mapFrame <- expand.grid(x = myLons, y = myLats)
#  mapFrame$y <- rev(mapFrame$y)
#  # form a frame with the new values and the data
#  tempFrame <- data.frame(sst = viirsSST$data$sst, lat = mapFrame$y, lon = mapFrame$x)
#  mycolor <- colors$temperature
#  w <- map_data("worldHires", ylim = c(30., 42.), xlim = c(-128, -114))
#  ggplot(data = tempFrame, aes(x = lon, y = lat, fill = sst)) +
#      geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#      geom_raster(interpolate = FALSE) +
#      scale_fill_gradientn(colours = mycolor, na.value = NA) +
#      theme_bw() + ylab("latitude") + xlab("longitude") +
#      coord_fixed(1.3, xlim = c(-128, -114),  ylim = c(30., 42.)) + ggtitle("Latest VIIRS 3-day SST")
#  

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  require("ggplot2")
#  require("rerddap")
#  viirsSST1 <- griddap(sstInfo, latitude = c(36., 36.),
#                       longitude = c(-126., -126.),
#                       time = c('2015-01-01','2015-12-31'), fields = 'sst')
#  tempTime <- as.Date(viirsSST1$data$time, origin = '1970-01-01', tz = "GMT")
#  tempFrame <- data.frame(time = tempTime, sst = viirsSST1$data$sst)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  ggplot(tempFrame, aes(time, sst)) +
#    geom_line() +
#    theme_bw() +
#    ylab("sst") +
#    ggtitle("VIIRS SST at (36N, 126W)")

## ----VHNCHla, eval = FALSE, echo = TRUE---------------------------------------
#  require("ggplot2")
#  require("mapdata")
#  require("rerddap")
#  chlaInfo <- info('erdVHNchla3day')
#  viirsCHLA <- griddap(chlaInfo, latitude = c(41., 31.),
#                       longitude = c(-128., -115), time = c('last','last'),
#                       fields = 'chla')

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  mycolor <- colors$chlorophyll
#  w <- map_data("worldHires", ylim = c(30., 42.), xlim = c(-128, -114))
#  ggplot(data = viirsCHLA$data, aes(x = longitude, y = latitude, fill = log(chla))) +
#    geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#    geom_raster(interpolate = FALSE) +
#    scale_fill_gradientn(colours = mycolor, na.value = NA) +
#    theme_bw() + ylab("latitude") + xlab("longitude") +
#    coord_fixed(1.3, xlim = c(-128, -114),  ylim = c(30., 42.)) +
#    ggtitle("Latest VIIRS 3-day Chla")

## ----eval = FALSE, echo = TRUE------------------------------------------------
#  require("rerddap")
#  dataInfo <- rerddap::info('hawaii_d90f_20ee_c4cb')
#  xpos <- c(135.25, 240.25)
#  ypos <- c(20.25, 60.25)
#  zpos <- c(70.02, 70.02)
#  tpos <- c('2010-12-15', '2010-12-15')
#  soda70 <- griddap(dataInfo,  longitude = xpos, latitude = ypos,
#                    time = tpos, depth = zpos, fields = 'temp' )
#  

## ----soda70Plot, eval = FALSE, echo = TRUE------------------------------------
#  require("ggplot2")
#  require("mapdata")
#  xlim <- c(135, 240)
#  ylim <- c(20, 60)
#  my.col <- colors$temperature
#  ## Must do a kludge to remove countries that wrap and mess up the plot
#  w1 <- map("world2Hires", xlim = c(135, 240), ylim = c(20, 60), fill = TRUE, plot = FALSE)
#  remove <- c("UK:Great Britain", "France", "Spain", "Algeria", "Mali", "Burkina Faso", "Ghana", "Togo")
#  w <- map_data("world2Hires", regions = w1$names[!(w1$names %in% remove)], ylim = ylim, xlim = xlim)
#  myplot <- ggplot() +
#      geom_raster(data = soda70$data, aes(x = longitude, y = latitude, fill = temp), interpolate = FALSE) +
#      geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#      theme_bw() + scale_fill_gradientn(colours = my.col, na.value = NA, limits = c(-3,30), name = "temperature") +
#      ylab("latitude") + xlab("longitude") +
#      coord_fixed(1.3, xlim = xlim, ylim = ylim) +
#      ggtitle(paste("70m temperature ", soda70$data$time[1]))
#  myplot

## ----NAtlSSS, eval = FALSE, echo = TRUE---------------------------------------
#  require("rerddap")
#  urlBase <- "https://erddap.marine.ie/erddap/"
#  parameter <- "sea_surface_salinity"
#  sssTimes <- c("last", "last")
#  sssLats <- c(48.00625, 57.50625)
#  sssLons <- c(-17.99375, -1.00625)
#  dataInfo <- rerddap::info("IMI_NEATL", url = urlBase)
#  NAtlSSS <- griddap(dataInfo, longitude = sssLons, latitude = sssLats, time = sssTimes, fields = parameter, url = urlBase)

## ----NAtlSSSplot, eval = FALSE, echo = TRUE-----------------------------------
#  require("ggplot2")
#  require("mapdata")
#  xlim <- c(-17.99375, -1.00625)
#  ylim <- c(48.00625, 57.50625)
#  my.col <- colors$salinity
#  w <- map_data("worldHires", ylim = ylim, xlim = xlim)
#  myplot <- ggplot() +
#      geom_raster(data = NAtlSSS$data, aes(x = longitude, y = latitude, fill = sea_surface_salinity), interpolate = FALSE) +
#      geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#      theme_bw() + scale_fill_gradientn(colours = my.col, na.value = NA, limits = c(34, 36), name = "salinity") +
#      ylab("latitude") + xlab("longitude") +
#      coord_fixed(1.3, xlim = xlim, ylim = ylim) +
#      ggtitle(paste("salinity", NAtlSSS$data$time[1]))
#  myplot

## ----IFREMER, eval = FALSE, echo = TRUE---------------------------------------
#  require("rerddap")
#  urlBase <- "https://www.ifremer.fr/erddap/"
#  parameter <- "PSAL"
#  ifrTimes <- c("2019-05-15", "2019-05-15")
#  ifrLats <- c(30., 50.)
#  ifrLons <- c(-140., -110.)
#  ifrDepth <- c(75., 75.)
#  dataInfo <- rerddap::info("CORIOLIS_GLOBAL_NRTOA_OBS_TIME_SERIE_PSAL", url = urlBase)
#  ifrPSAL <- griddap(dataInfo, longitude = ifrLons, latitude = ifrLats, time = ifrTimes, depth = ifrDepth,  fields = parameter, url = urlBase)

## ----ifrPSALplot, eval = FALSE, echo = TRUE-----------------------------------
#  ## ggplot2 has trouble with unequal y's
#   require("akima")
#   require("dplyr")
#   require("ggplot2")
#   require("mapdata")
#    xlim <- c(-140, -110)
#    ylim <- c(30, 51)
#  ## ggplot2 has trouble with unequal y's
#    my.col <- colors$salinity
#    tempData1 <- ifrPSAL$data$PSAL
#    tempData <- array(tempData1 , 61 * 54)
#    tempFrame <- data.frame(x = ifrPSAL$data$longitude, y = ifrPSAL$data$latitude)
#    tempFrame$temp <- tempData
#    tempFrame1 <- dplyr::filter(tempFrame, !is.nan(temp))
#    myinterp <- akima::interp(tempFrame1$x, tempFrame1$y, tempFrame1$temp, xo = seq(min(tempFrame1$x), max(tempFrame1$x), length = 61), yo = seq(min(tempFrame1$y), max(tempFrame1$y), length = 54))
#    myinterp1 <- expand.grid(x = myinterp$x, y = myinterp$y)
#    myinterp1$temp <- array(myinterp$z, 61 * 54)
#    w <- map_data("worldHires", ylim = ylim, xlim = xlim)
#   myplot <- ggplot() +
#      geom_raster(data = myinterp1, aes(x = x, y = y, fill = temp), interpolate = FALSE) +
#      geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#      theme_bw() + scale_fill_gradientn(colours = my.col, na.value = NA, limits = c(32, 35), name = "salinity") +
#      ylab("latitude") + xlab("longitude") +
#      coord_fixed(1.3, xlim = xlim, ylim = ylim) + ggtitle(paste("salinity at 75 meters",ifrPSAL$data$time[1] ))
#   myplot

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  require("rerddap")
#  hydroInfo <- info('siocalcofiHydroCast')

## ----calCOFI, eval = FALSE, echo = TRUE---------------------------------------
#  require("rerddap")
#  calcofi.df <- tabledap(hydroInfo, fields = c('cst_cnt',  'date', 'year', 'month', 'julian_date', 'julian_day', 'rpt_line', 'rpt_sta', 'cruz_num', 'intchl', 'intc14', 'time'), 'time>=1984-01-01T00:00:00Z', 'time<=2014-04-17T05:35:00Z')

## ----calCOFInum, eval = FALSE, echo = TRUE------------------------------------
#  calcofi.df$cruz_num <- as.numeric(calcofi.df$cruz_num)
#  calcofi.df$intc14 <- as.numeric(calcofi.df$intc14)
#  calcofi.df$time <- as.Date(calcofi.df$time, origin = '1970-01-01', tz = "GMT")

## ----calCOFIPlot, eval = FALSE,  echo = TRUE----------------------------------
#  require("dplyr")
#  
#  # calculate cruise means
#  by_cruznum <- group_by(calcofi.df, cruz_num)
#  tempData <- select(by_cruznum, year, month, cruz_num, intchl, intc14)
#  CruiseMeans <- summarize(by_cruznum, cruisechl = mean(intchl, na.rm = TRUE), cruisepp = mean(intc14, na.rm = TRUE), year = median(year, na.rm = TRUE), month = median(month, na.rm = TRUE))
#  tempTimes <- paste0(CruiseMeans$year,'-',CruiseMeans$month,'-1')
#  cruisetimes <- as.Date(tempTimes, origin = '1970-01-01', tz = "GMT")
#  CruiseMeans$cruisetimes <- cruisetimes
#  # calculate monthly "climatologies"
#  byMonth <- group_by(CruiseMeans, month)
#  climate <- summarize(byMonth, ppClimate = mean(cruisepp, na.rm = TRUE), chlaClimate = mean(cruisechl, na.rm = TRUE))
#  # calculate anomalies
#  CruiseMeans$chlanom <- CruiseMeans$cruisechl - climate$chlaClimate[CruiseMeans$month]
#  CruiseMeans$ppanom <- CruiseMeans$cruisepp - climate$ppClimate[CruiseMeans$month]
#  # calculate mean yearly anomaly
#  byYear <- select(CruiseMeans, year)
#  tempData <- select(CruiseMeans, year, chlanom, ppanom )
#  byYear <- group_by(tempData, year)
#  yearlyAnom <- summarize(byYear, ppYrAnom = mean(ppanom, na.rm = TRUE), chlYrAnom = mean(chlanom, na.rm = TRUE))
#  yearlyAnom$year <- ISOdate(yearlyAnom$year, 01, 01, hour = 0)
#  ggplot(yearlyAnom, aes(year, chlYrAnom)) + geom_line() +
#    theme_bw() + ggtitle('yearly chla anom')
#  ggplot(yearlyAnom, aes(year, ppYrAnom)) + geom_line() +
#    theme_bw() + ggtitle('yearly pp anom')

## ----CPS Query, eval = FALSE, echo = TRUE-------------------------------------
#  require("rerddap")
#  CPSquery <- ed_search(query = 'CPS Trawl')
#  CPSquery$alldata[[1]]$summary
#  CPSquery$alldata[[1]]$tabledap
#  CPSquery$alldata[[1]]$dataset_id

## ----CPSINfo, eval = FALSE, echo = TRUE---------------------------------------
#  require("rerddap")
#  CPSinfo <- info('FRDCPSTrawlLHHaulCatch')

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  require("dplyr")
#  require("rerddap")
#  sardines <- tabledap(CPSinfo, fields = c('latitude',  'longitude', 'time', 'scientific_name', 'subsample_count'), 'time>=2010-01-01', 'time<=2012-01-01', 'scientific_name="Sardinops sagax"' )
#  sardines$time <- as.Date(sardines$time, origin = '1970-01-01', tz = "GMT")
#  sardines$latitude <- as.numeric(sardines$latitude)
#  sardines$longitude <- as.numeric(sardines$longitude)
#  sardine2010 <- filter(sardines, time < as.Date('2010-12-01'))

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  # get the dataset info
#  sstInfo <- info('erdMWsstdmday')
#  # get 201004 monthly sst
#  sst201004 <- griddap('erdMWsstdmday', latitude = c(22., 51.), longitude = c(220., 255), time = c('2010-04-16','2010-04-16'), fields = 'sst')
#  # get 201104 monthly sst
#  sst201104 <- griddap('erdMWsstdmday', latitude = c(22., 51.), longitude = c(220., 255), time = c('2011-04-16','2011-04-16'), fields = 'sst')

## ----CPSPlot, eval = FALSE, echo = TRUE---------------------------------------
#  # get polygons of coast for this area
#  w <- map_data("worldHires", ylim = c(22., 51.), xlim = c(220 - 360, 250 - 360))
#  # plot 201004 sst on the map
#  sardine2010 <- filter(sardines, time < as.Date('2010-12-01', origin = '1970-01-01', tz = "GMT"))
#  sardine2011 <- filter(sardines, time > as.Date('2010-12-01', origin = '1970-01-01', tz = "GMT"))
#  mycolor <- colors$temperature
#  p1 <- ggplot() +
#    geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#    geom_raster(data = sst201004$data, aes(x = (longitude - 360), y = latitude, fill = sst), interpolate = FALSE) +
#    scale_fill_gradientn(colours = mycolor, na.value = NA, limits = c(5,30)) +
#    theme_bw() + ylab("latitude") + xlab("longitude") +
#    coord_fixed(1.3, xlim = c(220 - 360, 250 - 360),  ylim = c(22., 51.))
#  
#  # plot 201104 sst on the map
#  p2 <- ggplot() +
#    geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#    geom_raster(data = sst201104$data, aes(x = (longitude - 360), y = latitude, fill = sst), interpolate = FALSE) +
#    geom_point(data = sardine2011, aes(x = longitude, y = latitude, colour = subsample_count)) +
#    scale_fill_gradientn(colours = mycolor, na.value = NA, limits = c(5,30)) +
#    theme_bw() + ylab("latitude") + xlab("longitude") +
#    coord_fixed(1.3, xlim = c(220 - 360, 250 - 360),  ylim = c(22., 51.))
#  p1 + geom_point(data = sardine2010, aes(x = longitude, y = latitude, colour = subsample_count)) + scale_colour_gradient(space = "Lab", na.value = NA, limits = c(0,80))
#  
#  p2 +   geom_point(data = sardine2011, aes(x = longitude, y = latitude, colour = subsample_count)) + scale_colour_gradient(space = "Lab", na.value = NA, limits = c(0,80))

## ---- eval = FALSE,  echo = TRUE----------------------------------------------
#  sardinops <- tabledap(CPSinfo, fields = c('longitude', 'latitude', 'time'),  'scientific_name="Sardinops sagax"')
#  sardinops$time <- as.Date(sardinops$time, origin = '1970-01-01', tz = "GMT")
#  sardinops$year <- as.factor(format(sardinops$time, '%Y'))
#  sardinops$latitude <- as.numeric(sardinops$latitude)
#  sardinops$longitude <- as.numeric(sardinops$longitude)

## ----sardinesPlot, eval = FALSE, echo = TRUE----------------------------------
#  xlim <- c(-135, -110)
#  ylim <- c(30, 51)
#  coast <- map_data("worldHires", ylim = ylim, xlim = xlim)
#  ggplot() +
#      geom_point(data = sardinops, aes(x = longitude, y = latitude, colour = year)) +
#      geom_polygon(data = coast, aes(x = long, y = lat, group = group), fill = "grey80") +
#      theme_bw() + ylab("latitude") + xlab("longitude") +
#      coord_fixed(1.3, xlim = xlim, ylim = ylim) +
#      ggtitle("Location of sardines by year in EPM Trawls")

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  # get location and station ID of NDBC buoys in a region
#  BuoysInfo <- info('cwwcNDBCMet')
#  locationBuoys <- tabledap(BuoysInfo, distinct = TRUE, fields = c("station", "longitude", "latitude"), "longitude>=-124", "longitude<=-121", "latitude>=37", "latitude<=47")
#  locationBuoys$latitude <- as.numeric(locationBuoys$latitude)
#  locationBuoys$longitude <- as.numeric(locationBuoys$longitude)

## ----NDBC, eval = FALSE,  echo = TRUE-----------------------------------------
#  xlim <- c(-130, -110)
#  ylim <- c(35, 50)
#  coast <- map_data("worldHires", ylim = ylim, xlim = xlim)
#  ggplot() +
#     geom_point(data = locationBuoys, aes(x = longitude , y = latitude, colour = factor(station) )) +
#     geom_polygon(data = coast, aes(x = long, y = lat, group = group), fill = "grey80") +
#     theme_bw() + ylab("latitude") + xlab("longitude") +
#     coord_fixed(1.3, xlim = xlim, ylim = ylim) +
#     ggtitle("Location of buoys in given region")
#  

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  buoyData <- tabledap(BuoysInfo, fields = c("time", "wspd"), 'station="46012"', 'time>=2012-01-01', 'time<=2013-01-01')
#  buoyData$wspd <- as.numeric(buoyData$wspd)
#  buoyData$time <- as.Date(buoyData$time, origin = '1970-01-01', tz = "GMT")

## ----NDBCTS, eval = FALSE, echo = TRUE----------------------------------------
#  ggplot(buoyData, aes(time, wspd)) +
#    geom_line() +
#    theme_bw() +
#    ylab("wind speed") +
#    ggtitle("Wind Speed in 2012 from buoy 46012 ")

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  urlBase <- "https://data.ioos.us/gliders/erddap/"
#  gliderInfo <- info("sp064-20161214T1913",  url = urlBase)
#  glider <- tabledap(gliderInfo, fields = c("longitude", "latitude", "depth", "salinity"), 'time>=2016-12-14', 'time<=2016-12-23', url = urlBase)
#  glider$longitude <- as.numeric(glider$longitude)
#  glider$latitude <- as.numeric(glider$latitude)
#  glider$depth <- as.numeric(glider$depth)

## ----glider, eval = FALSE, echo = TRUE----------------------------------------
#  require("plot3D")
#  scatter3D(x = glider$longitude , y = glider$latitude , z = -glider$depth, colvar = glider$salinity,              col = colors$salinity, phi = 40, theta = 25, bty = "g", type = "p",
#             ticktype = "detailed", pch = 10, clim = c(33.2,34.31), clab = 'Salinity',
#             xlab = "longitude", ylab = "latitude", zlab = "depth",
#             cex = c(0.5, 1, 1.5))

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  atnURL <- 'https://oceanview.pfeg.noaa.gov/erddap/'
#  atnInfo <- info('gtoppAT', url = atnURL)
#  atnData <- tabledap(atnInfo, fields = c("time", "longitude", "latitude"), 'toppID="1807001"', url = atnURL)
#  atnData$latitude <- as.numeric(atnData$latitude)
#  atnData$longitude <- as.numeric(atnData$longitude)
#  ncdcSST = array(NA_real_, dim = length(atnData$time))
#  ncdcSSTInfo = info('ncdcOisst2Agg')
#  for (i in 1:length(atnData$time)) {
#    extract <- griddap(ncdcSSTInfo, fields = 'sst', latitude = c(atnData$latitude[i], atnData$latitude[i]), longitude = c(atnData$longitude[i], atnData$longitude[i]), time = c(atnData$time[i], atnData$time[i]))
#  ncdcSST[i] <- extract$data$sst
#  }

## ---- eval = FALSE,  echo = TRUE----------------------------------------------
#  ylim <- c(32.5, 34)
#  xlim <- c(-119, -116.5)
#  mycolor <- colors$temperature
#  w <- map_data("worldHires", ylim = ylim, xlim = xlim)
#  alldata <- data.frame(sst = ncdcSST, longitude = atnData$longitude - 360, latitude = atnData$latitude)
#  z <- ggplot(alldata, aes(x = longitude, y = latitude)) +
#     geom_point(aes(colour = sst), size = .5)
#  z + geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#    theme_bw() +
#    scale_colour_gradientn(colours = mycolor, limits = c(16.9, 17.3), "SST") +
#    coord_fixed(1.3, xlim = xlim, ylim = ylim) + ggtitle("SST Along Track")
#  

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  urlBase <- 'https://coastwatch.pfeg.noaa.gov/erddap/'
#  nphInfo <- info('erdNph', url = urlBase)
#  nphData <- tabledap(nphInfo, fields = c("year", "maxSLP" ), 'month=2', 'year>=1987', url = urlBase)
#  nphData$maxSLP <- as.numeric(nphData$maxSLP)
#  urlBase <- 'https://oceanview.pfeg.noaa.gov/erddap/'
#  cohoInfo <- info('cciea_SM_CA_CO_ABND', url = urlBase)
#  cohoData <- tabledap(cohoInfo, fields = c("abundance_anomaly", "time"),  url = urlBase)
#  cohoData$abundance_anomaly <- as.numeric(cohoData$abundance_anomaly)
#  alldata <- data.frame(coho = cohoData$abundance_anomaly[1:27], maxSLP = nphData$maxSLP, year = nphData$year)

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  ggplot(alldata) + geom_line(aes(x = year, y = coho), colour = 'blue') + theme_bw() + ggtitle("coho abundance anomaly")
#  ggplot(alldata) + geom_line(aes(x = year, y = maxSLP), colour = 'red') + theme_bw() + ggtitle("MaxSLP")

## ----  eval = FALSE, echo = TRUE----------------------------------------------
#  ?cache_delete
#  ?cache_delete_all
#  ?cache_details
#  ?cache_list

## -----------------------------------------------------------------------------
require("ncdf4")
exampleFile <- system.file("extdata", "MWsstd1day.nc", package = "rerddap")
sstFile <- nc_open(exampleFile)

## -----------------------------------------------------------------------------
names(sstFile$dim)

## -----------------------------------------------------------------------------
sstFile$dim$longitude$vals

## -----------------------------------------------------------------------------
names(sstFile$var)

## -----------------------------------------------------------------------------
require("ncdf4")
day1SST <- ncvar_get(sstFile, "sst", start = c(1, 1, 1, 1), count = c(1, 1, -1, -1))

## -----------------------------------------------------------------------------
latMin <- which(sstFile$dim$latitude$vals == 30.0)
latMax <- which(sstFile$dim$latitude$vals == 30.5)
lonMin <- which(sstFile$dim$longitude$vals == 210.0)
lonMax <- which(sstFile$dim$longitude$vals == 210.5)

## -----------------------------------------------------------------------------
require("ncdf4")
day1SST <- ncvar_get(sstFile, "sst", start = c(lonMin, latMin, 1, 1), count = c( (lonMax - lonMin + 1), (latMax - latMin + 1), 1, 1 ))

## -----------------------------------------------------------------------------
require("ncdf4")
day1SST <- ncvar_get(sstFile, "sst", start = c(lonMax, latMin, 1, 1), count = c(1, 1, 1, -1 ))

## -----------------------------------------------------------------------------
latMin <- max(which(sstFile$dim$latitude$vals <= 30.1))
latMax <- min(which(sstFile$dim$latitude$vals >= 30.3))
lonMin <- max(which(sstFile$dim$longitude$vals <= 210.1))
lonMax <- min(which(sstFile$dim$longitude$vals >= 210.3))


## -----------------------------------------------------------------------------
require("ncdf4")
day1SST <- ncvar_get(sstFile, "sst", start = c(lonMin, latMin, 1, 1), count = c( (lonMax - lonMin + 1), (latMax - latMin + 1), 1, 1 ))

