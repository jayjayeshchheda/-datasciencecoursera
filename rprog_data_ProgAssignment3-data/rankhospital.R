outcome_data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

rankhospital <- function(state, outcome, num = "best") {

  # Check if the state and outcome are valid
  if (!state %in% outcome_data$State) {
    stop("Invalid state")
  }
  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid_outcomes) {
    stop("Invalid outcome")
  }
  
  # Select the relevant columns
  outcome_col <- ifelse(outcome == "heart attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                        ifelse(outcome == "heart failure", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
  
  # Filter the data for the specified state and outcome
  state_data <- subset(outcome_data, State == state & outcome_data[[outcome_col]] != "Not Available")
  
  # Convert the outcome column to numeric
  state_data[[outcome_col]] <- as.numeric(state_data[[outcome_col]])
  
  # Order the data by outcome and hospital name
  state_data <- state_data[order(state_data[[outcome_col]], state_data$Hospital.Name), ]
  
  # Determine the rank
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(state_data)
  } else if (!is.numeric(num) || num > nrow(state_data) || num < 1) {
    return(NA)
  }
  
  # Return the hospital name at the specified rank
  return(state_data[num, "Hospital.Name"])
}