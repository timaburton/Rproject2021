setwd("~/Desktop/RProject2021")
##Assisting Functions for Project Code##

##Convert all files into csv files from space/tab delimintated ones##
  #define set of files and loop 
  #load an original file 
  #write out data in .csv format 

Convert_csv<-function(dir){
  setwd(dir) #Error in file(file, "rt"): cannot open the connection
  path<-list.files(pattern=".txt")
  for (i in 1:length(path)){
    file<-paste0(dir, path[i])
    data<-read.table(file, header= TRUE, stringsAsFactors = FALSE)
    write.csv(data, file=sub(pattern=".txt", replacement=".csv"))
  }
}
  
##Compile all Data into a single csv file##
  #define our set 
  #open each file 
  #add columns 

compile<-function(dir, name){
  setwd(dir)
  csv_files<-dir(pattern='*(\\d+).csv', recursive=TRUE)
  for(i in 1:length(csv_files)){
    if(i==1){
      df<-read.csv(csv_files[i])
    }else{
      df<-rbind(df,read.csv(csv_files[i]))
    }
  }
  country<-gsub("*/screen_[0-9]{3}.csv","*",csv_files)
  df['country']<- country
  dayofYear<-gsub("screen_*.csv","*",csv_files)
  df['dayofYear']<-dayofYear
  write.csv(df, file=name)
}




# Summarize function (number 3 in supportingFunctions.R)

setwd("~/Desktop/Fall-2021/Biocomputing/")
# Assumes there is compiled data
allData <- read.table("allData.csv", sep = ",", stringsAsFactors = TRUE, header = TRUE)
head(allData)

# Write a function to summarize the compiled data set in terms of number of screens run, percent of patients screened 
# that were infected, male vs. female patients, and the age distribution of patients.

# Number of screens run
#another way: total_screens <- nrow(allData)
total_screens <- count(allData[,])
print(paste0("Number of screens run: ",total_screens))

# Percent of patients screened that were infected 
infected<-0
not_infected<-0
for(i in 1:nrow(allData)){
  if(sum(allData[i,3:12])>=1){
    infected<-infected+1
  }else{
    not_infected<-not_infected+1
  }
}
percent <- (infected/total_screens)*100
print(paste0("Percent of patients screened that were infected: ",percent,"%"))

# male vs female patients
gender<- data.frame(table(allData$gender))
colnames(gender)<-c('Gender','Freq')

male<-gender[2,2]
print(paste0("Number of male patients: ",male))
female<-gender[1,2]
print(paste0("Number of female patients: ",female))

library(ggplot2)
ggplot(data=gender, aes(x=Gender,y=Freq,fill=Gender))+
  geom_bar(stat="identity")+
  ylab("Number of Patients")+
  theme_classic()

# Include infected for male and female and add points to graph? Right now showing total number of each gender out of all patients screened 

# age distribution

