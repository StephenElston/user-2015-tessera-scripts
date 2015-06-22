require(datadr)
require(parallel)
require(ggplot2)
require(gridExtra)
require(hexbin)
## Create a datadr backend with a 4 core cluster
options(defaultLocalDiskControl = localDiskControl(makeCluster(3)))

## Create a plot of the fare vs. distance with 
## tip amount removed. 
## We need to add a transform.
no_toll <- function(x){
  x$no_toll <- x$total_amount - x$tolls_amount
  x
}

# test it on a subset
head(no_toll(taxi[[1]]$value))

# add the transformation
taxiNoToll  <- addTransform(taxi, no_toll)

# look at the result
taxiNoToll

# look at the name of the result
names(taxiNoToll)

## Look at the relationship between fare without tolls and distance traveled
trns <- function(x) log2(x + 1)
amount_vs_dist <- drHexbin("trip_distance", "no_toll", data = taxiNoToll, 
                           xbins = 150, shape = 1, xTransFn = trns, yTransFn = trns,
                           xRange = c(0, 100), yRange = c(0, 650))

plot(amount_vs_dist, trans = log, inv = exp, style = "colorscale", colramp = LinOCS, xlab = "Distance (log2 miles)", ylab = "Amount Paid Less Tolls (log2 dollars)")


## Look at just the fare without tolls or tip
amount_vs_dist <- drHexbin("trip_distance", "fare_amount", data = taxiNoToll, 
                           xbins = 150, shape = 1, xTransFn = trns, yTransFn = trns,
                           xRange = c(0, 100), yRange = c(0, 650))

plot(amount_vs_dist, trans = log, inv = exp, style = "colorscale", colramp = LinOCS, xlab = "Distance (log2 miles)", ylab = "Fare paid, no tip or tolls (log2 dollars)")

## Zooming in on Manhattan
xRange <- c(-74.03, -73.92)
yRange <- c(40.7, 40.82)
tmp <- addTransform(taxi, function(x) {
  subset(x, pickup_longitude < -73.92 & pickup_longitude > -74.03 &
           pickup_latitude > 40.7 & pickup_latitude < 40.82)
})


## Create a transfrom for a column which are rate codes by distance traveled.
library(geosphere)
taxi_code_dist <- taxi %>%
  addTransform(function(x) {
    # get log distance in miles between pickup and dropoff
    x$distance <- log2( distHaversine(
      x[,c("pickup_longitude", "pickup_latitude")],
      x[,c("dropoff_longitude", "dropoff_latitude")],
      r = 3963.1676))
    # define breakpoints for categorization
    dist_breaks <- seq(0, max(x$distance, na.rm = TRUE) + 1, by  = 0.5)
    # break distance into categories
    x$dist_code <- interaction(cut(x$distance, breaks = dist_breaks), 
                               as.factor(x$rate_code))
    x
  })

## Test the result
names(taxi_code_dist)
drAggregate(~dist_code, data = taxi_code_dist)

# Divide the ddf by the distance-rate_code categories. 
dataPath <- "C:\\Users\\Steve\\Documents\\R\\Tessera\\Data\\taxi_by_dist_code"
diskConn <- localDiskConn(dataPath, autoYes = TRUE, reset = TRUE, verbose = TRUE)

taxi_by_dist_code <- divide(taxi_code_dist, 
                       by = "dist_code", 
                       output = diskConn,
                       overwrite = TRUE,
                       update = TRUE)
names(taxi_by_dist_code)

## A pannel function for the data divided by distance and rate code
library(ggplot2)
xRange <- c(-74.03, -73.92)
yRange <- c(40.7, 40.82)
distCodePanel <- function(x) {
  ggplot(x, aes(pickup_longitude, pickup_latitude)) + 
    stat_binhex(bins = 300, rm.na = TRUE) +
    xlim(xRange) + ylim(yRange)
}
distCodePanel(taxi_by_dist_code[[6]]$value)

## Create a cognositic function
taxiCog <- function(x) { 
  list(
    meanFare = cogMean(x$fare_amount),
    meanToll = cogMean(x$tolls_amount),
    meanTipPercent = cogMean((x$tip_amount *100) / x$fare_amount),
    count = nrow(x)
  )
}

library(trelliscope)
vdbConn("taxi-dist_code_vdb", autoYes=TRUE)
makeDisplay(taxi_by_dist_code,
            name = "Lat and Lon of pickup for trips",
            desc = "Lat and Lon of pickup location for trips",
            panelFn = distCodePanel, 
            width = 400, height = 400, 
            cogFn = taxiCog,
            lims = list(x = "same")
)

## Remove trips with cash payments which don't record tips
dataPath <- "C:\\Users\\Steve\\Documents\\R\\Tessera\\Data\\taxi_by_dist_code2"
diskConn <- localDiskConn(dataPath, autoYes = TRUE, reset = TRUE, verbose = TRUE)
taxi_by_dist_code2 <- drLapply(taxi_by_dist_code, function(x) x$payment_type != "CSH",
                               combine = combDdo, output = diskConn)
updateAttributes(taxi_by_dist_code2)

## Look at tip percentage vs time of the day, distance and code
distCodeTimePanel <- function(x) {
  x$hour <- as.factor(substring(strftime(x$pickup_datetime, 
                                       format = "%Y-%m-%d %H:%M:%S"), 12, 13))
  x$tipPct <- (x$tip_amount * 100) / x$fare_amount
  ggplot(x, aes(hour, tipPct)) +
    geom_boxplot(na.rm = TRUE) + ylim(0, 100)
}

distCodeTimePanel(taxi_by_dist_code[[8]]$value)

library(trelliscope)
vdbConn("taxi-dist_code_tip_vdb", autoYes=TRUE)
makeDisplay(taxi_by_dist_code,
            name = "Tip percent by time of day",
            desc = "Tip percent by time of day",
            panelFn = distCodeTimePanel, 
            width = 400, height = 400, 
            cogFn = taxiCog)

