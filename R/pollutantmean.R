pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  ## Set a variable to read only the column containing the pollutant data
  CC <- if (pollutant == "nitrate") {
    c("NULL", "NULL", NA, "NULL")
  } else if (pollutant == "sulfate") {
    c("NULL", NA, "NULL", "NULL")
  } 
  
  ## Construct a list of files names
  files <- sprintf ("%s/%03d.csv",directory,id)
  
  ## Read the pollutant data and unlist to compute the mean
  x <- lapply (files, read.csv, colClasses = CC)
  mean (unlist(x), na.rm = TRUE)
  
}