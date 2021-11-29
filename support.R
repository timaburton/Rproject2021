setwd("C:/Users/megan/Desktop/RProject/RProject2021")

uses <- list.files(("../RProject2021/countryY"), pattern = ".txt", full.name = TRUE)
#change files in the directory that are .txt files into .csv files
for (i in 1:length(uses)){
  input<-uses[i]
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  data = read.delim(input, header = TRUE)   
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
}

#add all csv files into a single file with two added columns: country and DayofYear
files <- function(dir){
  #grep for the two countries directory
  dirs <- list.dirs(("../RProject2021/"))
  used <- list.files(path = dirs, pattern = ".csv", full.names = TRUE)
  for (i in 1:length(used)){
      #add country X to country row of file
      #add country Y to country row of file
    #add not containing screen_ (numbers) to dayofyear row of file
    if (!exists("dataset")){
      dataset <- read.csv(used[i], header=TRUE)
    }
    if (exists("dataset")){
    tempory <-read.csv(used[i], header=TRUE)
    dataset <-unique(rbind(dataset, tempory))
    rm(tempory)
    }
    
    #if usable contains country X
   # if (grepl("countryX", used[i])==TRUE){
   #   dataset$country <- "country X"
   # }
    #if usable contains country y 
    if (grepl("countryY", used[i])==TRUE){
      dataset$country <- "country Y"
    }
  }
  write.csv(dataset,file="alls.csv", col.names = TRUE)
}

#




