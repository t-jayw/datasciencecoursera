rankall <- function(outcome, num = 'best') {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  outcome_df <- read.csv(grep('outcome-of-care', 
                              list.files(recursive = TRUE), 
                              value=TRUE), 
                         stringsAsFactors=FALSE,
                         colClasses="character")
  
  if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){stop("invalid outcome")}
  
  outcome_col <- switch(outcome, 
                        "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                        "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                        "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                        "invalid outcome")
  
  outcome_df <- outcome_df[,c("State", "Hospital.Name", outcome_col)]
  outcome_df[,outcome_col] <- suppressWarnings(as.numeric(outcome_df[,outcome_col]))
  outcome_df <- outcome_df[complete.cases(outcome_df),]
  
  split_df <- split(outcome_df, outcome_df$State)
  
  processed_split <- sapply(split_df, function(x, num){
    ordered <- x[order(x[,outcome_col], x$Hospital.Name),]
    
    if(is.numeric(num) & num < nrow(x)){
      return (ordered$Hospital.Name[num])
    }
    else if(num == 'best') {
      return (ordered$Hospital.Name[1])
    }
    else if(num == 'worst') {
      return (ordered$Hospital.Name[nrow(x)])
    }
    else {return(NA)}
  }, num)
  
  return_df <- data.frame('state' = names(processed_split),
                          'hospital' = unname(processed_split))
  return(return_df)
}
