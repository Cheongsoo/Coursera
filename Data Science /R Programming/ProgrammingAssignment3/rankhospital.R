rankhospital <- function(state, outcome, num = "best"){   
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- data.frame(data[,2], #name 
                         data[,7], #state
                         data[,11], #heart attack
                         data[,17], # heart failure
                         data[,23], stringsAsFactors = FALSE) #pneumonia
  colnames(hospital) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
  chosen_state <- hospital[which(hospital[, "state"] == state), ] #select the state
  chosen_state <- chosen_state[order(chosen_state$name),] #sort by alphabetical order
  chosen_outcome <- as.numeric(chosen_state[ , outcome]) #select the outcome from the state
  ## Check that state and outcome are valid
  if (!state %in% hospital[, "state"]) { stop('invalid state') } 
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) { stop('invalid outcome')}
  else if (num == "best") {
    best_outcome <- chosen_state[which(chosen_outcome == min(chosen_outcome, na.rm = TRUE)),]["name"]
    best_outcome[order(best_outcome)[1], ] }
  else if (num == "worst") {
    best_outcome <- chosen_state[which(chosen_outcome == max(chosen_outcome, na.rm = TRUE)),]["name"]
    best_outcome[order(best_outcome)[1], ] }
  else if (is.numeric(num)) {
    chosen_state <- chosen_state[order(chosen_outcome, decreasing = FALSE), ]
    chosen_state[num,"name"]
  }
  else { stop('NA') }
  
    
}
  
  
  
  
  
  
  
rankhospital("TX", "heart failure", 4)  
rankhospital("MD", "heart attack", "worst")
rankhospital("WY", "pneumonia", "worst")

