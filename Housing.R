## Make sure the required packages are loaded
require(datadr)
require(trelliscope)
require(housingData)

## Create a datadr backend with a 4 core cluster
require(parallel)
options(defaultLocalDiskControl = localDiskControl(makeCluster(4)))

## Create a ddf from the housing data and explore
housingDdf <- ddf(housing)
housingDdf
names(housingDdf)
getKeys(housingDdf)

## Divide the housing data by county and state
byCounty <- divide(housingDdf, 
                   by = c("county", "state"),
                   update = TRUE)
byCounty
getKeys(byCounty)[[1]]

## Try some other keys; state and time (month)
byState <- divide(housingDdf, 
                   by = "state",
                   update = TRUE)
byState
getKeys(byState)[[1]]

byMonth <- divide(housingDdf, 
                  by = "time",
                  update = TRUE)
byMonth
getKeys(byMonth)[[1]]

## Further explore the data
length(byCounty)
names(byCounty)
class(byCounty)
summary(byCounty)

# Function to calculate a linear model and extract 
# the slope coeficient
lmCoef <- function(x) {
  coef(lm(medListPriceSqft ~ time, data = x))[2]
}

## Test the function
lmCoef(byCounty[[1]]$value)

## Add the transform and test
byCountySlope <- addTransform(byCounty, lmCoef)
byCountySlope[[1]]

## Create another function, add it as a transform and test
totalSold <- function(x) {
  sum(x$nSold, na.rm=TRUE)
}
byCountySold <- addTransform(byCounty, totalSold)
byCountySold[[20]]

## And another function, transform and test
meanPriceDiff <- function(x) {
  mean(x$medListPriceSqft - x$medSoldPriceSqft, na.rm=TRUE)
}
byCountyPriceDiff <- addTransform(byCounty, meanPriceDiff)
byCountyPriceDiff[[20]]

## Recombine and check the slope coeficients
countySlopes <- recombine(byCountySlope, 
                          combine=combRbind)
head(countySlopes)

## Recombine other ddfs with transformations
totalSoldDf <- recombine(byCountySold, 
                         combine=combRbind)
totalSoldDf[1:20,]

Diffs <- recombine(byCountyPriceDiff, 
                         combine=combRbind)
Diffs[1:20,]
class(Diffs)

## This option does not work.
#DiffsList <- recombine(byCountyPriceDiff, 
#                       combine=collect)
#str(DiffsList)

DiffsDdo <- recombine(byCountyPriceDiff, 
                       combine=combDdo)
DiffsDdo[[20]]


## Investigsate some new data and create ddfs and divide them.
## First perform the operations on geoCounty
geoDdf <- ddf(geoCounty)
geoDdf
names(geoDdf)
getKeys(geoDdf)

## Divide the housing data by county and state
geoByCounty <- divide(geoDdf, 
                   by = c("county", "state"),
                   update = TRUE)
geoByCounty
getKeys(geoByCounty)[[1]]

## Next perform the operations on wikiCounty
wikiDdf <- ddf(wikiCounty)
wikiDdf
names(wikiDdf)
getKeys(wikiDdf)

## Divide the housing data by county and state
## This part does not work..........
#wikiByCounty <- divide(wikiDdf, 
#                     by = c("county", "state"),
#                      update = TRUE)

## But this does work??
wikiByCounty <- divide(wikiCounty, 
                       by=c("county", "state"))
wikiByCounty
getKeys(wikiByCounty)[[1]]

## Now join all this together
joinedData <- drJoin(housing=byCounty, 
                     slope=byCountySlope, 
                     geo=geoByCounty, 
                     wiki=wikiByCounty)
joinedData
class(joinedData)
getKeys(joinedData)[1]
joinedData[[176]]

## Some data subsetting
names(joinedData[[2884]]$value)
joinedData[[2884]]$value$geo
joinedData[[2884]]$value$wiki

## Filter out elements with no housing sales data from the list
## This does not work.
#joinedData <- drFilter(joinedData,
#                       function(k,v){!is.null(v$housing)})

## But this does.
joinedData <- drFilter(joinedData,
                       function(v){!is.null(v$housing)})
joinedData[[1]]  ## Note the missing values
joinedData[[20]]  ## There is something wrong with these data!


## Alternatively we can create new columns in the original ddf 
## with a transformation. The function needs to assign the computed
## quantity to a new new column in the ddf and return the ddf.
## Note the 
totalSold2 <- function(x) {
  x$totalSold <- sum(x$nSold, na.rm=TRUE)
  x
}
byCounty <- addTransform(byCounty, totalSold2)
byCounty[[20]]$value$totalSold

## Add another column with a transform
meanPriceDiff2 <- function(x) {
  x$meanPriceDiff <- mean(x$medListPriceSqft - x$medSoldPriceSqft, na.rm=TRUE)
  x
}
byCounty <- addTransform(byCounty, meanPriceDiff2)
byCounty[[20]]$value$meanPriceDiff

## Check what we have now noting the new columns in the ddf
byCounty
names(byCounty)
