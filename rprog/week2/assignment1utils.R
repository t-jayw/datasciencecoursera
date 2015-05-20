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

countNulls <- function(df){
  return(sum(complete.cases(df)))
}

readAndCountComplete <- function(path) {
  df <- read.csv(path)
  countNulls(df)
}
