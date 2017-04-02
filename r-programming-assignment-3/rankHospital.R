

rankhospital <- function(state, outcome, num = "best") {
  outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Check valid state
  raw_states = outcome_df[, 7]
  valid_states = unique(raw_states)
  if (!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  #Check valid outcome
  if (!(outcome =="heart attack" || outcome =="heart failure" || outcome =="pneumonia")){
    stop("invalid outcome")  
  }
  if (outcome == "heart attack") {
    sort_on_col = 11 
  }
  if (outcome == "heart failure") {
  }
  if (outcome == "pneumonia") {
    sort_on_col = 23
  }  
  # Find all rows that match the state
  outcome_df = outcome_df[outcome_df$State == state,]
  # Convert col to sort into numeric
  outcome_df[, sort_on_col] <- as.numeric(outcome_df[, sort_on_col] )
  # Filter out NAs for the outcome col that matters
  outcome_df <- outcome_df[complete.cases(outcome_df[, sort_on_col]),]
  # Sort first on outcome col and then on name col
  outcome_df <- outcome_df[
                  with (outcome_df, order(outcome_df[, sort_on_col],outcome_df[, 2])),
                ]
  if (num == "best") {
    outcome_df[1,2]    
  }
  else if (num == "worst") {
    last <- tail(outcome_df, n=1) 
    last[1,2]
  }
  else {
    outcome_df[num, 2]  
  }
}
