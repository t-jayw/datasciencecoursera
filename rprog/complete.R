source("assignment1utils.R")

complete <- function(directory, id = 1:10){
  file_range <- sapply(id, longifyNumber)
  file_list = sapply(file_range, constructPath, directory = directory)
  
  counted_completes <- unname(sapply(file_list, readAndCountComplete))
  
  return_df <- as.data.frame(cbind(id,counted_completes))
  
  colnames(return_df) <- c("id", "nobs")
  
  return(return_df)
}
  
### TESTING
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)





