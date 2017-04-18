rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[, 11] <- as.numeric(data[, 11])
        data[, 17] <- as.numeric(data[, 17])
        data[, 23] <- as.numeric(data[, 23])
        
        
        ## Check that state and outcome are valid
        
        states <- unique(data[,"State"])
        if (!state %in% states)
                stop('invalid state')
        
        outcomeKey <- gsub(" ", ".", outcome)
        ##outcomes <- c("heart attack", "heart failure", "pneumonia")
        outcomesMappings <- data.frame("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        if (!outcomeKey %in% names(outcomesMappings))
                stop('invalid outcome')
        
        col <- outcomesMappings[1,outcomeKey]
        
        stateDataIndexes = data[ , "State"] == state
        stateData <- data[stateDataIndexes, ]
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        stateDataNoNa <- stateData[!is.na(stateData[ , col]), ]
        bestHospitalIndexes = order(stateDataNoNa[ , col],stateDataNoNa[ , "Hospital.Name"])
        hospitalNames <- stateDataNoNa[bestHospitalIndexes, "Hospital.Name"]
        
        if (num == "best")
                hospitalNames[1]
        else if (num == "worst")
                tail(hospitalNames, n=1)
        else
                hospitalNames[num]
}