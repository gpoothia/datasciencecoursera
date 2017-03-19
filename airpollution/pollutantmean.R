#https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

pollutantmean <- function(directory, pollutant, id){
  setwd(directory)
  filenames <- system("ls .",intern=TRUE)
  accu = vector()
  for(i in 1:length(filenames)){
    if (i %in% id) {
      print(filenames[i])
      df <- read.csv(filenames[i])
      col <- df[[pollutant]]
      good <- complete.cases(col)
      filter_col = col[good]
      # all NA col will result in col[good] being a logical
      if (class(filter_col) != class(TRUE)) {
        accu = c(accu, filter_col)
      }
    }
  }
  mean(accu)
}

setwd("/Users/gauravpoothia/code/repos/coursera-data-science/airpollution/")
print(pollutantmean("specdata", "nitrate", 1:10))