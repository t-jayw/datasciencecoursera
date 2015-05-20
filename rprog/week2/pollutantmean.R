source("assignment1utils.R")

pollutantmean <- function(directory, pollutant, id = 1:332){
  # Used a utility functions to pad with '0's for filenames
  file_range <- sapply(id, longifyNumber)
  file_list = sapply(file_range, constructPath, directory=directory)
  
  # Read in files from file_list and append into one DF
  agg_csv <- do.call("rbind", 
        lapply(file_list, function(x) read.csv(x, stringsAsFactors = FALSE))
    )
  
  # Remove NA and calculate means
  mean_calc <- mean(agg_csv[!is.na(agg_csv[ ,pollutant]), pollutant])
  return(mean_calc)
}

### TESTING ###

pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064

pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706

pollutantmean("specdata", "nitrate", 23)
## [1] 1.281
