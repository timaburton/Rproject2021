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
  for(i in 1:length(files)){
    files<-list.files(pattern=".csv$", recursive=TRUE)
    temp<-read.csv(files[i], header=TRUE) #Object 'files' not found
    dataset<-rbind(temp)
    country<-print("Country*") 
    dataset$country<-country
    dayofYear<-print("120:175") #Fix it so dayofYear shows up 
    dataset$dayofYear
    write.csv(dataset, file=name)
  }
}


<<<<<<< HEAD
# Supporting function that takes complied data set and returns summary of number of screens run
# percent of patients screened that were infected, male vs female patients, and the age distribution of patients. 
=======
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





# Summarize function (number 3 in supportingFunctions.R)

# Set working directory
# Function is designed to be used in the same directory as the compiled data set 
setwd("~/Desktop/Fall-2021/Biocomputing/")

# Usage: summarize("filename.csv")

summarize <- function(filename){
  filename_table <- read.table(filename, sep = ",", stringsAsFactors = TRUE, header = TRUE)
  
  # Summary of number of screens run 
  total_screens <- nrow(filename_table)
  print(paste0("Number of screens run: ",total_screens))
  
  # Percent of patients screened that were infected 
  infected<-0
  not_infected<-0
  for(i in 1:nrow(filename_table)){
    if(sum(filename_table[i,3:12])>=1){
      infected<-infected+1
    }else{
    }
  }
  percent <- (infected/total_screens)*100
  print(paste0("Percent of patients screened that were infected: ",round(percent, digits = 2),"%"))
  
  # Male vs female patients
  gender<- data.frame(table(filename_table$gender))
  colnames(gender)<-c('Gender','Freq')
  
  male<-gender[2,2]
  print(paste0("Number of male patients: ",male))
  female<-gender[1,2]
  print(paste0("Number of female patients: ",female))
  
  # Load plot libraries
  library(ggplot2)
  library(cowplot)
  plot1 <- ggplot(data=gender, aes(x=Gender,y=Freq,fill=Gender))+
    geom_bar(stat="identity")+
    ylab("Number of Patients")+
    ggtitle("Male vs Female")+
    scale_fill_manual(values = c('lightblue','darkblue'))+
    theme_classic()
  
  # Age distribution
  plot2 <- ggplot(data=filename_table,aes(x=age))+
    geom_histogram(binwidth = 10, fill="lightblue", color="darkblue")+ 
    ylab("Number of Patients")+
    xlab("Age")+
    ggtitle("Age Distribution")+
    theme_classic() 
  
  # Display both summary graphs in console
  plot_grid(plot1, plot2, ncol=2, nrow =1, width = 8, height = 5)
}

# Call function
summarize("allData.csv")