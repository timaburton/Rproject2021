setwd("~/Desktop/RProject2021")
##Assisting Functions for Project Code##

##Convert all files into csv files from space/tab delimintated ones 
  #define set of files and loop 
  #load an original file 
  #write out data in .csv format 
Convert_csv<-function(dir){
  path<-list.files(path=dir)
  for (x in 1:length(path)){
    df<-fread(path[x])
    data<-read.table(files[x], header = TRUE)
    write.csv(data,
              file=sub(pattern="\\.txt$", replacement = ".csv", x=x))
  }
}
  
##Compile all Data into a single csv file 
  #define our set 
  #open each file 
  #add columns 
compile<-function(){
  read.csv("*.csv", header = TRUE, sep=",")
  #compile all data into one file#
  file$country=country
  file$dayofYear=dayofYear 
  cbind(, dayofYear, country)
  #argument to handle NAs 
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