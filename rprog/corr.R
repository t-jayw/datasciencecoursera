
corr <- function(directory, threshold=0) {
  file_list <- list.files(path = directory, pattern = "*.csv")
  file_paths <- unname(sapply(file_list, function(x) paste(directory, x, sep="/")))
  
  corr_vec <- numeric()
  
  for(file in file_paths){
    in_file <- read.csv(file)
    nobs <- sum(complete.cases(in_file))
    if(nobs > threshold) {
      cor_df <- in_file[complete.cases(in_file), ]
      corr_vec <- c(corr_vec, cor(cor_df$sulfate, cor_df$nitrate))
    }
  }
  return(corr_vec)
}

### TESTING ###

cr <- corr("specdata", 150)
head(cr)
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630


cr <- corr("specdata", 400)
head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630


cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
length(cr)
## [1] 0


cr <- corr("specdata")
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
length(cr)
## [1] 323