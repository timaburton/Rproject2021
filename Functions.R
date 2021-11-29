setwd("~/Desktop/RProject2021")
##Assisting Functions for Project Code##

#Convert all files into csv files 
Convert_csv<-function(x){
  for x in directory{
    read.tables(file=, header=TRUE, sep="\t")
  }
  setwd(x)
   data<-Read.table("*.csv",header=TRUE, sep="\t", sep=" ")
  return(data.csv)
}
  
#Compile all Data into a single csv file 
compile<-function(){
  read.csv("*.csv", header = TRUE, sep=",")
  #compile all data into one file#
  file$country=country
  file$dayofYear=dayofYear 
  cbind(, dayofYear, country)
}

#Summarize the compiled daya in terms of number of screen runs, percent patients infected 
  #male vs. female patients, age distribution 
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