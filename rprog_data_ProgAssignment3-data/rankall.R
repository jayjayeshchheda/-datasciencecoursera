outcome_data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

rankall <- function(outcome, num = "best") {
  
  # Check if the outcome is valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid_outcomes) {
    stop("Invalid outcome")
  }
  
  # Select the relevant columns
  outcome_col <- ifelse(outcome == "heart attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                        ifelse(outcome == "heart failure", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
  
  # Initialize a data frame to store the results
  results <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  
  # Get the list of states
  states <- unique(outcome_data$State)
  
  # Loop through each state and find the hospital with the specified rank
  for (state in states) {
    # Filter the data for the specified state and outcome
    state_data <- subset(outcome_data, State == state & outcome_data[[outcome_col]] != "Not Available")
    
    # Convert the outcome column to numeric
    state_data[[outcome_col]] <- as.numeric(state_data[[outcome_col]])
    
    # Order the data by outcome and hospital name
    state_data <- state_data[order(state_data[[outcome_col]], state_data$Hospital.Name), ]
    
    # Determine the rank
    if (num == "best") {
      rank <- 1
    } else if (num == "worst") {
      rank <- nrow(state_data)
    } else if (!is.numeric(num) || num > nrow(state_data) || num < 1) {
      rank <- NA
    } else {
      rank <- num
    }
    
    # Get the hospital name at the specified rank
    hospital_name <- if (!is.na(rank)) state_data[rank, "Hospital.Name"] else NA
    
    # Add the result to the data frame
    results <- rbind(results, data.frame(hospital = hospital_name, state = state, stringsAsFactors = FALSE))
  }
  
  return(results)
}