rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcome_df <- read.csv(grep('outcome-of-care', 
                              list.files(recursive = TRUE), 
                              value=TRUE), 
                         stringsAsFactors=FALSE,
                         colClasses="character")
  
  # Process Args
  if(!(state %in% outcome_df[,"State"])){stop("invalid state")}
  if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){stop("invalid outcome")}
  # Switch user input to outcome column names
  outcome_col <- switch(outcome, 
                        "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                        "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                        "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                        "invalid outcome")
  
  # Isolate state/outcome data into new DF, process, order and rank
  state_df <- outcome_df[outcome_df$State == state, c("State","Hospital.Name",outcome_col)]
    
  suppressWarnings(state_df[,outcome_col] <- as.numeric(state_df[ ,outcome_col]))
  
  state_df <- state_df[complete.cases(state_df),]
  
  state_df <- state_df[order(state_df[ ,outcome_col], state_df[ , "Hospital.Name"] ), ]
  
  state_df$Rank <- 1:nrow(state_df)
  
  
  processNum <- function(num){
    if(is.numeric(num)){
      if(num <= max(state_df$Rank)){
        num
      }
      else {NA}
    }
    else if(is.character(num)){
      if(num %in% c('best', 'worst')){
        switch(num,
               best=min(state_df$Rank),
               worst=max(state_df$Rank))
      }
      else {NA}
    }
    else {NA}
  }
  
  process_return <- function(num){
    if(is.na(processNum(num))){ processNum(num) }
    else{(state_df[state_df$Rank == processNum(num),2])}
  }
  
  return(process_return(num))
}
  
  
  
  
  
  
  