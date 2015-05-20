source("assignment1utils.R")

pollutantmean <- function(directory, pollutant, id = 3:12){
  file_range <- sapply(id, longifyNumber)
  file_list = sapply(file_range, constructPath, directory = directory)
  agg_csv <- do.call("rbind", 
        lapply(file_list, function(x) read.csv(x, stringsAsFactors = FALSE))
    )
  
  mean_calc <- mean(agg_csv[!is.na(agg_csv[,pollutant]), pollutant])
  return(mean_calc)
}

### TESTING
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

