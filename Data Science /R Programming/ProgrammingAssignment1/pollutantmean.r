pollutantmean <- function(directory, pollutant, id = 1:332) {
        data <- list.files(directory, full.names = TRUE)
        pollutant_data <- data.frame()
        for (i in id) {
        csv_file <- read.csv(data[i])
        pollutant_data <- rbind(pollutant_data, csv_file)
        }
        
        mean(pollutant_data[, pollutant], na.rm = TRUE)
}




