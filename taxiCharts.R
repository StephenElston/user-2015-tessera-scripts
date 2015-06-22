require(datadr)
require(parallel)
require(ggplot2)
require(gridExtra)
## Create a datadr backend with a 4 core cluster
options(defaultLocalDiskControl = localDiskControl(makeCluster(3)))

## Plot medallion use frequency
mft <- summary(taxi)$medallion$freqTable

plot(sort(mft$Freq),
     ylab = "Medallion frequency",
     main = "Frequency of occurrance of medallions for top and bottom 5000")
abline(v = 5000, lty = 2, col = "gray")

## Plot the hack license use frequency
hft <- summary(taxi)$hack_license$freqTable

plot(hft$Freq,
     ylab = "Hack license frequency",
     main ="Frequncy of use for hack licenses for top and bottom 5000")
abline(v = 5000, lty = 2, col = "gray")

## Compute a list of the quantiles of some of the variables
varName <- c("passenger_count",
             "trip_time_in_secs",
             "trip_distance", 
             "total_amount", 
             "tip_amount")
quantList <- lapply(varName, function(var) drQuantile(taxi, var = var, tails = 1))

## Create and test a function to create the quantile plot
quant.plot <- function(quant, var.name, ylim){
  p1 <- ggplot(quant, aes(fval, q)) +
    geom_point() +
    ggtitle(paste("Quantile plot of", var.name)) +
    ylab(var.name) + xlab("Proportion")
  p2 <- ggplot(subset(quant, q < ylim), 
               aes(fval, q)) +
    geom_point() +
    ggtitle(paste("Quantile plot of", var.name)) +
    ylab(var.name) + xlab("Proportion")
  grid.arrange(p1, p2, ncol=2)
}
quant.plot(quantList[[1]], "passenger_count", 10)

## Lets plot the variables
ylims <- c(10,
           3000,
           25,
           100,
           40)

Map(quant.plot, quantList, varName, ylims)

## Drill down on tips by payment type
tip_quant <- drQuantile(taxi, var = "tip_amount", by = "payment_type", tails = 1)

ggplot(subset(tip_quant, q < 20), 
       aes(fval, q, group = payment_type)) +
  geom_point(aes(colour = factor(payment_type))) +
  ggtitle(paste("Quantile plot of tip amount")) +
  ylab("Tip amount") + xlab("Proportion")



## Create hexbin plot of taxi data.
pickup_latlon <- drHexbin("pickup_longitude", "pickup_latitude", data = taxi, xbins = 300, shape = 1.4)

library(hexbin)
plot(pickup_latlon, trans = log, inv = exp, style = "centroids", xlab = "longitude", ylab = "latitude", legend = FALSE)

## Look at the relationship between fare and distance traveled
trns <- function(x) log2(x + 1)
amount_vs_dist <- drHexbin("trip_distance", "total_amount", data = taxi, xbins = 150, shape = 1, xTransFn = trns, yTransFn = trns)

plot(amount_vs_dist, trans = log, inv = exp, style = "colorscale", colramp = LinOCS, xlab = "Distance (log2 miles)", ylab = "Total Amount Paid (log2 dollars)")

## Plot with another color scale
plot(pickup_latlon, trans = log, inv = exp, style = "colorscale", xlab = "longitude", ylab = "latitude", colramp = LinOCS)

## Zooming in on Manhattan
xRange <- c(-74.03, -73.92)
yRange <- c(40.7, 40.82)
tmp <- addTransform(taxi, function(x) {
  subset(x, pickup_longitude < -73.92 & pickup_longitude > -74.03 &
           pickup_latitude > 40.7 & pickup_latitude < 40.82)
})
pickup_latlon_manhat <- drHexbin("pickup_longitude", "pickup_latitude", data = tmp, xbins = 300, shape = 1.4, xRange = xRange, yRange = yRange)

## And plot with different color scales
plot(pickup_latlon_manhat, trans = log, inv = exp, style = "centroids", xlab = "longitude", ylab = "latitude", legend = FALSE)
plot(pickup_latlon_manhat, trans = log, inv = exp, style = "colorscale", xlab = "longitude", ylab = "latitude", colramp = LinOCS)


