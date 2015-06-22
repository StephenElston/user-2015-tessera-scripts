require(datadr)
require(parallel)
require(ggplot2)
require(gridExtra)
require(hexbin)
## Create a datadr backend with a 4 core cluster
options(defaultLocalDiskControl = localDiskControl(makeCluster(3)))


## Compute some 
library(geosphere)
taxi_dist <- taxi %>%
  addTransform(function(x) {
    # get log distance in miles between pickup and dropoff
    x$distance <- log2( distHaversine(
      x[,c("pickup_longitude", "pickup_latitude")],
      x[,c("dropoff_longitude", "dropoff_latitude")],
      r = 3963.1676))
    # define breakpoints for categorization
    dist_breaks <- seq(0, max(x$distance, na.rm = TRUE) + 1, by  = 0.5)
    # break distance into categories
    x$distance_cat <- cut(x$distance, breaks = dist_breaks)
    x
  })

names(taxi_dist)

# look at the distribution of our new categorical variable
drAggregate(~distance_cat, data = taxi_dist)

# Divide the ddf by the distance category. 
dataPath <- "C:\\Users\\Steve\\Documents\\R\\Tessera\\Data\\taxi_by_dist"
diskConn <- localDiskConn(dataPath, autoYes = TRUE, reset = TRUE, verbose = TRUE)

taxi_by_dist <- divide(taxi_dist, 
                       by = "distance_cat", 
                       output = diskConn,
                       overwrite = TRUE,
                       update = TRUE)

## Look at some statistics by distance category.
## First look for differences in tip percentages by
## distance category. Create a transform, add the
## transform to the ddf, and test it
meanTipPrct <- function(x) {
  mean((x$tip_amount * 100) / x$fare_amount, na.rm=TRUE)
}
tipPrctByDist <- addTransform(taxi_by_dist, meanTipPrct)
tipPrctByDist[[8]]$value
tipPrctByDistComb <- recombine(tipPrctByDist, combine=combRbind)
tipPrctByDistComb

## Have a look at tolls paid by
## distance category. Create a transform, add the
## transform to the ddf, and test it
meanToll <- function(x) {
  mean(x$tolls_amount, na.rm=TRUE)
}
meanTollByDist <- addTransform(taxi_by_dist, meanToll)
meanTollByDist[[8]]$value
meanTollByDistComb <- recombine(meanTollByDist, combine=combRbind)
meanTollByDistComb
