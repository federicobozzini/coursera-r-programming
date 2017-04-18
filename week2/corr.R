corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  res <- vector()
  id = 1:332
  for (i in id) {
    filename <- sprintf("%03d", i)
    filepath <- paste(directory, "/", filename, ".csv", sep="")
    data <- read.csv(filepath)
    completeData <- data[!is.na(data$nitrate) & !is.na(data$sulfate), ]
    nobs <- nrow(completeData)
    if (nobs > threshold)
      res <- c(res, cor(completeData$nitrate, completeData$sulfate))
  }
  res
}