data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE)
outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE)
outcome2 <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = TRUE)


hospital <- data.frame(data[,2], #name 
                       data[,7], #state
                       data[,11], #heart attack
                       data[,17], # heart failure
                       data[,23], stringsAsFactors = FALSE) #pneumonia
colnames(hospital) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
chosen_state <- hospital[which(hospital[, "state"] == "AR"), ] #select the state
chosen_state <- chosen_state[order(chosen_state["heart attack"], chosen_state["hospital"]),] #sort by alphabetical order
chosen_outcome <- as.numeric(chosen_state[ , "heart attack"]) #select the outcome from the state
chosen_state[chosen_outcome[20],]

chosen_state[20,][1:2]

?data.frame
da <- split(hospital, hospital$state)
head(da)
which(hospital$state == "AK")
hospital[99:115,]
library(plyr)
arrange(hospital, "state")
??arragne
hospital_heart <- hospital[order(hospital$state, hospital$hospital), ]
split_state <- split(hospital_heart, hospital$state)
rank_state <- lapply(split_state, function(x) {x[20, "heart attack" ]})
head(rank_state,10)
head(hospital[20,], 10)
split_state[20,]

heart <- hospital[order(hospital$`heart attack`, hospital$hospital),]
split_state <- split(heart, heart$state)
rank_state <- lapply(split_state, function(x) {x[20, 1:2 ]})
head(rank_state, 10)
rank_data <- do.call(rbind, rank_state)
rownames(rank_data) <- NULL
head(rank_data,10)
#head
hospital[,"state"]
chosen_state <- hospital[which(hospital[, "state"] == "VT"), ]
success <- (order(chosen_state$`heart failure`, chosen_state$hospital, decreasing = TRUE))
tail(chosen_state[success,])

#tail(rankall("pneumonia", "worst"), 3)
hospital[,"state"]
which(hospital[, "state"] == "WI")
chosen_state <- hospital[which(hospital[, "state"] == "WY"), ]
fail <- (order(chosen_state$pneumonia, chosen_state$hospital))
tail(chosen_state[fail,],6)

chosen_outcome <- as.numeric(chosen_state[ , "pneumonia"])
best_outcome <- chosen_state[which(chosen_outcome == max(chosen_outcome, na.rm = TRUE)),][c("hospital", "state")]
best_outcome <- chosen_state[which(chosen_outcome == max(chosen_outcome, na.rm = TRUE)),][re]
best_outcome <- chosen_state[which(chosen_outcome == max(chosen_outcome, na.rm = TRUE)),][1:2]
best_outcome[order(best_outcome)[1], ] 

chosen_state[which(chosen_outcome == max(chosen_outcome, na.rm = TRUE)),]

re <- c("hospital", "state")


chosen_state <- hospital[which(hospital[, "state"] == "WI"), ] 
chosen_state[which(chosen_state[, "heart failure"] == fail), ]

which(chosen_state[, "heart failure"] == 10)
fail

#REGULAR
fail <- hospital[order(hospital$`heart failure`, hospital$hospital),]
split_state <- split(heart, heart$state)
rank_state <- lapply(split_state, function(x) {x[20, 1:2 ]})
head(rank_state, 10)
rank_data <- do.call(rbind, rank_state)
rownames(rank_data) <- NULL
head(rank_data,10)