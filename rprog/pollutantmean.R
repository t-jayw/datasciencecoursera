longifyNumber <- function(number, min_char = 3){
  if(nchar(number) < min_char){
    needed <- min_char - nchar(number)
    padding <- paste(rep(0,needed), collapse = "")
    longNumber <- paste(padding, number, sep = "")
    return(longNumber)
  }
  else{
    return(number)
  }
}

constructPath <- function(filename, file_type = '.csv', directory){
  return(paste(directory,"/",filename,file_type, sep = ""))
}

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

