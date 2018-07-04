data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospital <- data.frame(data[,2], #name 
                       data[,7], #state
                       data[,11], #heart attack
                       data[,17], # heart failure
                       data[,23], stringsAsFactors = FALSE) #pneumonia
colnames(hospital) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
chosen_state <- hospital[which(hospital[, "state"] == "TX"), ] #select the state
chosen_state <- chosen_state[order(chosen_state$name),]
chosen_outcome <- as.numeric(chosen_state[ , "heart failure"]) #select the outcome from the state
#best_outcome <- chosen_state[which(chosen_outcome == min(chosen_outcome, na.rm = TRUE)),]["name"]
#best_outcome[order(best_outcome), ]
#which(hospital[, "state"] == "TX")

nm <- order(chosen_outcome, decreasing = FALSE)
#head(nm)
#head(chosen_outcome)
#head(nm)
#head(chosen_state[nm, ]["name"])
chosen_state <- chosen_state[order(chosen_outcome, decreasing = FALSE), ]
chosen_state[4,"name"]
chosen_state[4, ]["name"]
class(chosen_state[,"name"])

head(chosen_state[order(chosen_state$name),])
nm[4]
?sort


