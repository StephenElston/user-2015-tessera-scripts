## Make sure the required packages are loaded
require(datadr)
require(trelliscope)
require(housingData)

## Create a pannel function
saleSoldPannel <- function(xx){
  yy <- xx$housing
  ggplot(yy) + 
    geom_point(aes(time, medListPriceSqft)) +
    geom_point(aes(time, medSoldPriceSqft), color = "red", shape = 17) 
}

## Test the pannel function
saleSoldPannel(joinedData[[176]]$value)

## Set up the visualization db connection
vdbConn("housing_vdb", autoYes=TRUE)

## Make the display and view it
## This does not work with ggplot2!!
makeDisplay(joinedData,
            name = "list_sold_vs_time_datadr",
            desc = "List and sold price over time",
            panelFn = saleSoldPannel, 
            width = 400, height = 400, 
            lims = list(x = "same")
)

## Or try with a Lattice package pannel function.
timePanel <- function(x) {
  xyplot(medListPriceSqft + medSoldPriceSqft ~ time,
         data = x$housing, auto.key = TRUE, 
         ylab = "Price / Sq. Ft.")
}
timePanel(joinedData[[176]]$value)

makeDisplay(joinedData,
            name = "list_sold_vs_time_datadr",
            desc = "List and sold price over time",
            panelFn = timePanel, 
            width = 400, height = 400, 
            lims = list(x = "same")
)


## Create a cognositic function
priceCog <- function(x) { 
  st <- getSplitVar(x, "state")
  ct <- getSplitVar(x, "county")
  zillowString <- gsub(" ", "-", paste(ct, st))
  list(
    slope = cog(x$slope, desc = "list price slope"),
    meanList = cogMean(x$housing$medListPriceSqft),
    meanSold = cogMean(x$housing$medSoldPriceSqft),
    lat = cog(x$geo$lat, desc = "county latitude"),
    lon = cog(x$geo$lon, desc = "county longitude"),
    wikiHref = cogHref(x$wiki$href, desc="wiki link"),
    zillowHref = cogHref(
      sprintf("http://www.zillow.com/homes/%s_rb/", 
              zillowString), 
      desc="zillow link")
  )
}

## Make a display using the cognositic function as
## an argument
makeDisplay(joinedData,
            name = "list_sold_vs_time_datadr2",
            desc = "List and sold price with cognostics",
            panelFn = timePanel, 
            cogFn = priceCog,
            width = 400, height = 400, 
            lims = list(x = "same")
)

## Add another transform to use in a cognostic
diffsDf <- recombine(byCountyPriceDiff, 
                      combine=combRbind)
diffsDf[1:20,]

diffsDdf <- divide(Diffs, 
                   by = c("county", "state"),
                   update = TRUE)

joinedData <- drJoin(housing=byCounty, 
                     slope=byCountySlope, 
                     priceDiff=diffsDdf,
                     geo=geoByCounty, 
                     wiki=wikiByCounty)
joinedData
class(joinedData)
getKeys(joinedData)[1]
joinedData[[176]]
