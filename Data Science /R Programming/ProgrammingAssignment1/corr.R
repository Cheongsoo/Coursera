#Write a function that takes a directory of data files and a threshold for complete cases and 
#calculates the correlation between sulfate and nitrate for monitor locations 
#where the number of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.





corr <- function(directory, threshold = 0) {
        files <- list.files(directory, full.names = TRUE)
        correlation <- NULL
        for(i in seq_along(files)) {
                read_data <- read.csv(files[i])
                complete <- complete.cases(read_data)
                
                if(sum(complete) > threshold) {
                        data <- data.frame(read_data[which(complete),]['sulfate'], read_data[which(complete),]['nitrate'])
                        corr_data <- cor(data$sulfate, data$nitrate)
                        correlation <- c(correlation, corr_data)
                } 
                
        }
        correlation
}


