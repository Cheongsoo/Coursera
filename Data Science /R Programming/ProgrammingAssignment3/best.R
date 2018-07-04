best <- function(state, outcome) {
  ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    hospital <- data.frame(data[,2], #name 
                           data[,7], #state
                           data[,11], #heart attack
                           data[,17], # heart failure
                           data[,23], stringsAsFactors = FALSE) #pneumonia
    colnames(hospital) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
    if (!state %in% hospital[ , "state"]) {
      stop("invalid state")
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
      stop('invalid outcome')
    } 
  ## Return hospital name in that state with lowest 30-day death
      else {
        chosen_state <- hospital[which(hospital[, "state"] == state), ] #select the state
        chosen_outcome <- chosen_state[ , outcome] #select the outcome from the state
        best_outcome <- chosen_state[which(chosen_outcome == min(chosen_outcome, na.rm = TRUE)),]["name"]
      }
    ## rate
    best_outcome[order(best_outcome)[1], ]
  }

best("TX", "heart failure")
best("BB", "heart attack")
best("NY", "hert attack")


