#Write a function that reads a directory full of files and 
#reports the number of completely observed cases in each data file. 
#The function should return a data frame where the first column is the name of the file 
#and the second column is the number of complete cases. 





complete <- function(directory, id = 1:322) {
        files_list <- list.files(directory, full.names = TRUE)
        z <- length(id)
        data <- data.frame(id = numeric(z), nobs = numeric(z))
        for (i in seq_along(id)) {
                found_id <- read.csv(files_list[id[i]])
                data$id[i] <- i
                data$nobs[i] <- sum(complete.cases(found_id))
        }
        data
}



