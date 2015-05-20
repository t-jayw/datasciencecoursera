source("complete.R")
source("assignment1utils.R")

corr <- function(directory, threshold) {
  file_list <- list.files(path = directory, pattern = "*.csv")
  
  file_paths <- unname(sapply(file_list, function(x) paste(directory, x, sep="/")))
  
  complete_df <- complete(directory, 1:length(file_list))
  
  complete_ids <- complete_df$id[complete_df$nobs > threshold]
  
  threshold_file_paths <- file_paths[complete_ids]

  for (file in threshold_file_paths) {
    if (!exists("corr_df")){
      corr_df <- read.csv(file)
    }
    else {
      temp_dataset <-read.csv(file)
      corr_df<-rbind(corr_df, temp_dataset)
      rm(temp_dataset)
    }
  }
  
  correlations <- for (i in complete_ids){
##PROCESS TEH FILES AND CORRS ONE BY ONE    
    
  return(cor(corr_df$sulfate, corr_df$nitrate, na.omit = TRUE))
}

cr <- corr("specdata",400)
head(cr)
