outcome_data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

best <- function(state, outcome) {

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
  
  # Check if there are any valid entries
  if (nrow(state_data) == 0) {
    stop("No data available for this state and outcome")
  }
  
  # Find the hospital with the lowest 30-day mortality rate
  best_hospital <- state_data[which.min(state_data[[outcome_col]]), "Hospital.Name"]
  
  # Handle ties by sorting alphabetically and selecting the first hospital
  tied_hospitals <- state_data[state_data[[outcome_col]] == min(state_data[[outcome_col]]), "Hospital.Name"]
  best_hospital <- sort(tied_hospitals)[1]
  
  return(best_hospital)
}
