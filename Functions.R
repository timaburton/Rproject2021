setwd("~/Desktop/RProject2021")
##Assisting Functions for Project Code##

##Convert all files into csv files from space/tab delimintated ones##
  #define set of files and loop 
  #load an original file 
  #write out data in .csv format 

Convert_csv<-function(dir){
  setwd(dir) #Error in file(file, "rt"): cannot open the connection
  path<-list.files(dir)
  for (i in path){
    file<-paste0(dir, path[i])
    data<-read.table(file, header = TRUE, stringsAsFactors = FALSE)
    write.csv(data,
              file=sub(pattern="\\.txt$", replacement = ".csv", x=x))
  }
}
  
##Compile all Data into a single csv file##
  #define our set 
  #open each file 
  #add columns 

compile<-function(dir, name, where){
  setwd(dir) #Alter so it compiles both countries 
  for(data in list.files()){
    temp<-read.csv(data, header=TRUE)
    dataset<-rbind(temp)
    country<-print(where) 
    dataset$country<-country
    dayofYear<-print(120:175) #Fix it so dayofYear shows up 
    dataset$dayofYear
    write.csv(dataset, file=name)
  }
}

##Summarize the compiled daya in terms of number of screen runs, percent patients infected 
  ##male vs. female patients, age distribution 
  #1. assumes compiled data 
  #subset 
  #count 
  #sum 
  #print or plot in informative way 

summarize<-function(i){
  data<-read.table(file=i, header=TRUE, sep=",")
  n<-count(data[,])
  print("number of screen runs", n)
  #number of rows with at least 1 present 
  #count male vs. female
  print("male", "female")
  #age
  summary(data$age)
}