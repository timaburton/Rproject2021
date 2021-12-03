setwd("~/Desktop/RProject2021")
##Assisting Functions for Project Code##

##Convert all files into csv files from space/tab delimintated ones##
Convert_csv<-function(dir){
  setwd(dir) 
  path<-list.files(path=dir, pattern=".txt", recursive = TRUE)
  for (i in 1:length(path)){
    file<-path[i]
    out<-paste0(gsub("\\.txt$","",file),".csv")
    data<-read.table(file, header= TRUE)
    write.csv(data, file=out)
  }
}
  
##Compile all Data into a single csv file##
#Only running for one file? also getting NA for column data 
compile<-function(dir, name){
  setwd(dir)
  csv_files<-list.files(path="~/Desktop/RProject2021",pattern='*(\\d+).csv', recursive=TRUE, full.names = FALSE)
  dayofYear<-"dayofYear"
  country<-"Country"
  input<-readline(prompt = "What do you want to do with rows with NAs: remove, get warning, keep ?")
  for(i in csv_files){
    if(input=="remove"){
      df<-read.csv(csv_files[i], header = TRUE, stringsAsFactors = FALSE)
      df<-na.omit(df)
      df[,dayofYear]<-gsub("country[A-Z]{1}/screen_*.csv","*",csv_files[i])
      df[,country]<-gsub("country*/screen_[0-9]{3}.csv","*",csv_files[i])
    }else if(input=="get warning"){
      df<-read.csv(csv_files[i], header = TRUE, stringsAsFactors = FALSE)
      df[,dayofYear]<-gsub("country[A-Z]{1}/screen_*.csv","*",csv_files[i])
      df[,country]<-gsub("country*/screen_[0-9]{3}.csv","*",csv_files[i])
      print("Warning: Data contains NAs")
    }else if(input=="keep"){
      df<-read.csv(csv_files[i], header = TRUE, stringsAsFactors = FALSE)
      df[,dayofYear]<-gsub("country[A-Z]{1}/screen_*.csv","*",csv_files[i])
      df[,country]<-gsub("country*/screen_[0-9]{3}.csv","*",csv_files[i])
    }
  }
  x<-rbind(df)
  write.csv(x, file=name)
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

