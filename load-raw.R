dataPath <- "C:\\Users\\Steve\\Documents\\R\\Tessera\\Data"
fileName <- "trip_1.csv"

load.raw <- function(path, file.name, nrows = -1){
  ## Function reads the rather large raw taxi data file 
  ## as a data.table object.
  require(data.table)
  file.name <- file.path(path,file.name)
  fread(file.name, nrows = nrows)
}


taxi <- load.raw(dataPath, fileName)
taxi <- taxi[pickup_datetime < "2013-01-09 00:00:00", ]
taxi <- data.frame(taxi)
names(taxi) <- gsub("X\\.", "", names(taxi))
write.csv(taxi, file = paste(dataPath, "\\", "taxiOneWeek.csv", sep =""),
          row.names = FALSE)


