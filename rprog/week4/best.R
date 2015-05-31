best <- function(state, outcome) {
  outcome_df <- read.csv(grep('outcome-of-care', 
                              list.files(recursive = TRUE), 
                              value=TRUE), 
                         stringsAsFactors=FALSE,
                         colClasses="character")
  
  # Check for valid Args
  if(!(state %in% outcome_df[,7])){stop("invalid state")}
  if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){stop("invalid outcome")}
  
  # Switch user input to outcome column names
  outcome_col <- switch(outcome, 
           "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
           "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
           "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  # Isolate state/outcome data into new DF
  state_df <- outcome_df[outcome_df$State == state, c("State","Hospital.Name",outcome_col)]

  # Convert values to types I need (ignore warnings about introducing NAs)
  suppressWarnings(state_df[,outcome_col] <- as.numeric(state_df[ ,outcome_col]))
  
  # Pull rows where outcome is equal to minum outcome
  best_df <- state_df[which(state_df[,outcome_col] == min(state_df[,outcome_col], 
                                                          na.rm = TRUE)), ]
  
  # Return the 'min' hospital name incase more than 1 is returned above
  return(min(best_df$Hospital.Name))
}


### Tests ###

# best("TX", "heart attack")
# "CYPRESS FAIRBANKS MEDICAL CENTER"

# best("TX", "heart failure")
# "FORT DUNCAN MEDICAL CENTER"

# best("MD", "heart attack")
# "JOHNS HOPKINS HOSPITAL, THE"

# best("MD", "pneumonia")
# "GREATER BALTIMORE MEDICAL CENTER"

# best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state

# best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
  
  
  
