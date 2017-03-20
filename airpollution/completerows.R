#https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

complete <- function(directory, id){
  cdf <- data.frame(id=numeric(),
                   nobs=numeric())
  setwd(directory)
  filenames <- system("ls .",intern=TRUE)
  for(i in 1:length(filenames)){
    if (i %in% id) {
      df <- read.csv(filenames[i])
      good <- complete.cases(df)
      good_rows = df[good,][]
      #print(nrow(good_rows))
      cdf <- rbind(cdf, c(i, nrow(good_rows)))
    }
  }
  cdf  
}

setwd("/Users/gauravpoothia/code/repos/coursera-data-science/airpollution/")
print(complete("specdata", 3))