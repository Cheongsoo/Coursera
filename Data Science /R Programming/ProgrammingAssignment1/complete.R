complete <- function(directory, id = 1:322) {
        files_list <- list.files(directory, full.names = TRUE)
        z <- length(id)
        dat <- data.frame(id = numeric(z), nobs = numeric(z))
        for (i in seq_along(id)) {
                found_id <- read.csv(files_list[id[i]])
                dat$id[i] <- id[i]
                dat$nobs[i] <- sum(complete.cases(found_id$sulfate, found_id$nitrate))
        }
        dat
}



