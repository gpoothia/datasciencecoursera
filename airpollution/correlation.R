#https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

corr <- function(directory, threshold = 0){
  vec <-  vector('numeric')
  setwd(directory)
  filenames <- system("ls .",intern=TRUE)
  for(i in 1:length(filenames)){
      df <- read.csv(filenames[i])
      good <- complete.cases(df)
      good_rows = df[good,][]
      if (nrow(good_rows) >=  threshold){
        sulph <- df[["sulfate"]]
        nit <- df[["nitrate"]]
        #print(cor(sulph,nit))
        vec <- c(vec, cor(sulph, nit, use = "complete.obs"))
      }
  }
  vec
}

setwd("/Users/gauravpoothia/code/repos/coursera-data-science/airpollution/")
cr <- corr("specdata", 150)
head(cr)
summary(cr)