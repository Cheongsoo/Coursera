rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hosp <- data.frame(data[,2], #name 
                         data[,7], #state
                         data[,11], #heart attack
                         data[,17], # heart failure
                         data[,23], stringsAsFactors = FALSE) #pneumonia
  colnames(hosp) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
 
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) { stop('invalid outcome')}
  ## For each state, find the hospital of the given rank
  else if (num == "best") {
    sort_hospital <- order(hosp[,outcome], hosp[,"hospital"]) #sort outcome and hospital first
    sort_hospital <- hosp[sort_hospital, ]
    
    outcome_hospital <- sort_hospital[,c("hospital","state",outcome)]
    outcome_hospital[,3] <- as.numeric(outcome_hospital[,3])
    outcome_hospital <- outcome_hospital[order(outcome_hospital[,outcome], decreasing = FALSE), ]
    
    hospital_state <- split(outcome_hospital, outcome_hospital[,"state"])
    result <- lapply(hospital_state, function(x) { x[1,1:2]}) 
    result <- do.call(rbind, result) }
  else if (num == "worst") {
    sort_hospital <- order(hosp[,outcome], hosp[,"hospital"]) #sort outcome and hospital first
    sort_hospital <- hosp[sort_hospital, ]
    
    outcome_hospital <- sort_hospital[,c("hospital","state",outcome)]
    outcome_hospital[,3] <- as.numeric(outcome_hospital[,3])
    outcome_hospital <- outcome_hospital[order(outcome_hospital[,outcome], decreasing = TRUE), ]
    
    hospital_state <- split(outcome_hospital, outcome_hospital[,"state"])
    result <- lapply(hospital_state, function(x) { x[1,1:2]}) 
    result <- do.call(rbind, result)}
  
  else if (is.numeric(num)) {
    sort_hospital <- order(hosp[,outcome], hosp[,"hospital"]) 
    sort_hospital <- hosp[sort_hospital, ]
    
    outcome_hospital <- sort_hospital[,c("hospital","state",outcome)]
    outcome_hospital[,3] <- as.numeric(outcome_hospital[,3])
    outcome_hospital <- outcome_hospital[order(outcome_hospital[,outcome], decreasing = FALSE), ]
    
    
    hospital_state <- split(outcome_hospital, outcome_hospital$state)
    
    result <- lapply(hospital_state, function(x) { x[num,1:2]}) #if sorted
    result <- do.call(rbind, result)
    result[,2]
    result[,2] <- rownames(result) #Insert state name to NA
  } else { stop('NA') }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  result
  }

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
