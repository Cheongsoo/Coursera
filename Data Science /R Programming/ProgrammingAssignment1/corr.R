corr <- function(directory, threshold = 0) {
        files <- list.files(directory, full.names = TRUE)
        correlation <- NULL
        for(i in seq_along(files)) {
                data <- read.csv(files[i])
                complete <- complete.cases(data)
                
                if(sum(complete) > threshold) {
                        data <- data.frame(data[which(complete),]['sulfate'], data[which(complete),]['nitrate'])
                        corr_data <- cor(data$sulfate, data$nitrate)
                        correlation <- c(correlation, corr_data)
                } 
                
        }
        correlation
}


