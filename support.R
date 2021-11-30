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
   if (grepl("countryX", used[i])==TRUE){
     dataset$country <- "country X"
   }
    #if usable contains country y 
    if (grepl("countryY", used[i])==TRUE){
      dataset$country <- "country Y"
    }
    
    dataset$DayOfYear <- used[i]
  }
  
  write.csv(dataset,file="alls.csv", col.names = TRUE)
}
allD <- read.csv("allData.csv", header = TRUE, stringsAsFactors = FALSE)

summary <- function(allD){
  #number of screens run
  total <- nrow(allD)
  print(paste("The total number of screens run was", as.character(total)))
  #number of infected patients
  numOfInfected <- 0
for (j in 1:total){
  for (i in 1:ncol(allD)){
    if(allD[j,i] == 1){
      numOfInfected = numOfInfected + 1
      break
    } else{
      numOfInfected = numOfInfected
    }
  }
}
  percent <- (numOfInfected/total)*100
  print(paste("The percent of infected patients is" , percent, "%"))
  #male vs female patients
  fem <- 0
  male <- 0
  for (j in 1:total){
    if(allD[j,1] == "female"){
      fem = fem +1
    }else{
      male =male +1
    }
  }
  print(paste("The number of female vs male patients:" , fem, "vs", male))
  #age distribution of patients
  
  
  #the number for each marker for each country
  for (i in 1:10) {
    assign(paste0("markerX", i), 0*i)
    assign(paste0("markerY", i), 0*i)
  } 
    for (i in 1:nrow(allD)){
      for (j in 3:12){
        for (k in 1:10){
          
      if(allD[i,13] == "X"){
      markerX1 <- markerX1 + as.numeric(allD[i,j])
      }
      if(allD[i,13]=="Y"){
        markerY1 <- markerY1 + as.numeric(allD[i,j])
      }
        }
      }  
  }
  
  #the first 10 days number of infected people in each country 
}
summary(allD)
allD[5,13]
