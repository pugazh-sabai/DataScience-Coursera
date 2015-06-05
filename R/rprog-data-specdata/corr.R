corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  ## Construct a list of files names
  fileList <- list.files(path=directory, pattern="*.csv")
  files <- sprintf ("%s/%s",directory,fileList)
  
  ## Read the pollutant data in a matrix format
  x <- sapply (files, read.csv, simplify = TRUE, USE.NAMES=TRUE)
  
  nobs <- NULL
  cr <- NULL
  
  for (i in 1:length(fileList)) {
    nobs <- c(nobs, sum(complete.cases(x[,i])))
    if (nobs[i] > threshold) {
      cr = c(cr, cor(unlist(x[,i][2]), unlist(x[,i][3]), use = "complete.obs"))
    }
  }
  cr
}