rankall <- function(outcome, num = "best") {
        ## Read outcome data
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[, 11] <- as.numeric(data[, 11])
        data[, 17] <- as.numeric(data[, 17])
        data[, 23] <- as.numeric(data[, 23])
        
        
        ## Check that outcome is valid
        
        outcomeKey <- gsub(" ", ".", outcome)
        ##outcomes <- c("heart attack", "heart failure", "pneumonia")
        outcomesMappings <- data.frame("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        if (!outcomeKey %in% names(outcomesMappings))
                stop('invalid outcome')
        
        col <- outcomesMappings[1,outcomeKey]
        
        statesData <- split(data, data[, "State"])
        
        orderByOutcome <- function(statesData){
                bestHospitalIndexes = order(statesData[ , col],statesData[ , "Hospital.Name"])
                res <- statesData[bestHospitalIndexes, c("Hospital.Name","State")]
                colnames(res) <- c("hospital", "state")
                res
        }
        
        res<- by(data, data[, "State"], orderByOutcome)

        
        if (num == "best")
                res2 <- lapply(res, "[", 1,1:2)
        else if (num == "worst")
                res2 <- lapply(res, tail, 1)
        else
                res2 <- lapply(res, "[", num,1:2)

        do.call(rbind.data.frame, res2)
}