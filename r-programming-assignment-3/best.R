

best <- function(state, outcome) {
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
  outcome_df <- outcome_df[order(outcome_df[, sort_on_col]),]
  # Extract min value
  min = outcome_df[1, sort_on_col]
  #Filter on min value
  outcome_df <- outcome_df[outcome_df[, sort_on_col] == min,]
  outcome_df <- outcome_df[order(outcome_df[, 2]),]
  outcome_df[1,2]
}
