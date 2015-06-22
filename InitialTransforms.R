require(datadr)
require(parallel)
## Create a datadr backend with a 4 core cluster
options(defaultLocalDiskControl = localDiskControl(makeCluster(3)))


load.ddf <- function(path, in.file, out.file, slash = "\\",
                     postTransFn = trans,
                     rowsPerBlock = 300000, 
                     overwrite = TRUE){
  ## Function creates a ddf from a .csv file of the 
  ## taxi data.
  require(datadr)
  in.file <- paste(path, slash, in.file, sep = "")
  out.file <- paste(path, slash, out.file, sep = "")
  disk.con <- localDiskConn(out.file, autoYes = TRUE)
  drRead.csv(in.file, 
             output = disk.con, 
             header = TRUE, 
             postTransFn = postTransFn,
             rowsPerBlock = rowsPerBlock,
             stringsAsFactors = FALSE,
             overwrite = overwrite) 
}

trans <- function(x) {
  # convert to POSIXct time
  require(lubridate)
  x$pickup_datetime <- fast_strptime(as.character(x$pickup_datetime), format = "%Y-%m-%d %H:%M:%S", tz = "EST")
  x$dropoff_datetime <- fast_strptime(as.character(x$dropoff_datetime), format = "%Y-%m-%d %H:%M:%S", tz = "EST")
  
  # set coordinates outside of NYC bounding box to NA
  nw <- list(lat = 40.917577, lon = -74.259090)
  se <- list(lat = 40.477399, lon = -73.700272)
  ind <- which(x$dropoff_longitude < nw$lon | x$dropoff_longitude > se$lon)
  x$dropoff_longitude[ind] <- NA
  ind <- which(x$pickup_longitude < nw$lon | x$pickup_longitude > se$lon)
  x$pickup_longitude[ind] <- NA
  ind <- which(x$dropoff_latitude < se$lat | x$dropoff_latitude > nw$lat)
  x$dropoff_latitude[ind] <- NA
  ind <- which(x$pickup_latitude < se$lat | x$pickup_latitude > nw$lat)
  x$pickup_latitude[ind] <- NA
  x
}

dataPath <- "C:\\Users\\Steve\\Documents\\R\\Tessera\\Data"
in.file <-  "taxiOneWeek.csv"
out.file <- "taxiddf"

## Create the taxi data set and update the attributes
taxi <- load.ddf(dataPath, in.file, out.file)
taxi <- updateAttributes(taxi)

