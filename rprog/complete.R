

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

constructPath <- function(filename, directory, file_type = '.csv'){
  return(paste(directory,"/",filename,file_type, sep = ""))
}

complete <- function(directory, id = 3:12){
  file_range <- sapply(id, longifyNumber)
  file_list = sapply(file_range, constructPath, directory = directory)
  
  N <- length(id)
  df <-  data.frame(id=rep(NA, N), obs=rep("", N), 
                    stringsAsFactors=FALSE)     
  for(i in id){
    id <- i
    file_name <- longifyNumber(i)
    file_path <- constructPath(file_name, directory = directory)
    file <- read.csv(file_path)
    nobs <- sum(complete.cases(file))
    df[i,] <- c(id, nobs)
  }
  return df
}

### TESTING
complete("specdata", 1:10)

complete("specdata", 23)



