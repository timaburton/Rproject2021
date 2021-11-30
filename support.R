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
  dis <- sort(allD$age, decreasing = TRUE)
  over100 <- 0
  zeroto20 <- 0
  twentyto40 <- 0
  fourtyto60 <- 0
  sixtyto80 <- 0
  eightyto100 <- 0
  for (i in 1:length(dis)){
    if (dis[i]> 100){
      over100 <- over100 +1
    }
    if (dis[i] <=100 & dis[i] > 80){
      eightyto100 = eightyto100 +1
    }
    if (dis[i] <= 80 & dis[i] > 60){
      sixtyto80 = sixtyto80 +1
    }
    if (dis[i] <=60 & dis[i] > 40) {
      fourtyto60 = fourtyto60 +1
    }
    if (dis[i] <= 40 & dis[i] > 20){
      twentyto40 = twentyto40 + 1
    }
    if(dis[i] <= 20){
      zeroto20 = zeroto20 +1
    }
  }
  agedis=matrix(0,nrow=0,ncol = 6)
  colnames(agedis) <- c("under 20", "20-40", "40-60", "60-80", "80-100", "over 100")
  ages <- c(zeroto20,twentyto40,fourtyto60,sixtyto80,eightyto100,over100)
  agedistribution <- rbind(agedis,ages)
  agedistribution

#number of people from each country with each marker 
      xcountry <- allD[allD$country == "X",]
      vectx <- c()
      for (i in 3:12){
      vectx <- c(vectx, sum(xcountry[,i]))
      #print(paste("The number of people from country X with marker" , i-2, "is", sum(xcountry[,i])))
        
      }
      ycountry <- allD[allD$country == "Y",]
      vecty <- c()
      for (i in 3:12){
        vecty <- c(vecty, sum(ycountry[,i]))
       # print(paste("The number of people from country Y with marker" , i-2, "is", sum(ycountry[,i])))
      }
#the number of infected people of each country
      numOfInfectedX <- 0
      for (j in 1:nrow(xcountry)){
        for (i in 1:ncol(xcountry)){
          if(xcountry[j,i] == 1){
            numOfInfectedX = numOfInfectedX + 1
            break
          } else{
            numOfInfectedX = numOfInfectedX
          }
        }
      }
      #print(paste("The number of infected patients in country X is" , numOfInfectedX))
      numOfInfectedY <- 0
      for (j in 1:nrow(ycountry)){
        for (i in 1:ncol(ycountry)){
          if(ycountry[j,i] == 1){
            numOfInfectedY = numOfInfectedY + 1
            break
          } else{
            numOfInfectedY = numOfInfectedY
          }
        }
      }
      #print(paste("The number of infected patients in country Y is" , numOfInfectedY))
      y <- c("country", "infected", "marker 1", "marker 2","marker 3","marker 4","marker 5","marker 6","marker 7",
             "marker 8","marker 9","marker 10")
      A=matrix(0,nrow=0,ncol=12)
      colnames(A) = y
      rowX <- c("X", numOfInfectedX, vectx)
      rowY <- c("Y", numOfInfectedY, vecty)
      B <- rbind(A, rowX)
      C <- rbind(B, rowY)
      summ <- as.data.frame(C)
      PatientAges<- as.data.frame(agedistribution)

#the first 5 days number of infected people in each country 
  
for (b in 120:125) {
  beginning <- allD[allD$dayofYear == b,]
  beginningY <- beginning[beginning$country == "Y",]
  numOfInfectedYinB <- 0
  for (j in 1:nrow(beginningY)){
    for (i in 1:ncol(beginningY)){
      if(ycountry[j,i] == 1){
        numOfInfectedYinB = numOfInfectedYinB + 1
        break
      } else{
        numOfInfectedYinB = numOfInfectedYinB
      }
    }
  }
  
  beginningX <- beginning[beginning$country == "X",]
  numOfInfectedXinB <- 0
  for (j in 1:nrow(beginningX)){
    for (i in 1:ncol(beginningX)){
      if(xcountry[j,i] == 1){
        numOfInfectedXinB = numOfInfectedXinB + 1
        break
      } else{
        numOfInfectedXinB = numOfInfectedXinB
      }
    }
  }
  print(paste("The number of people in country Y infected on day " , b, "is: " ,  numOfInfectedYinB))
  print(paste("The number of people in country X infected on day " , b, "is: " ,  numOfInfectedXinB))
}
      beginning <- allD[allD$dayofYear < 125,]
      beginningY <- beginning[beginning$country == "Y",]
      numOfInfectedYinB <- 0
      for (j in 1:nrow(beginningY)){
        for (i in 1:ncol(beginningY)){
          if(ycountry[j,i] == 1){
            numOfInfectedYinB = numOfInfectedYinB + 1
            break
          } else{
            numOfInfectedYinB = numOfInfectedYinB
          }
        }
      }
      
      beginningX <- beginning[beginning$country == "X",]
      numOfInfectedXinB <- 0
      for (j in 1:nrow(beginningX)){
        for (i in 1:ncol(beginningX)){
          if(xcountry[j,i] == 1){
            numOfInfectedXinB = numOfInfectedXinB + 1
            break
          } else{
            numOfInfectedXinB = numOfInfectedXinB
          }
        }
      }
      print(paste("The total number of people infected in country Y on day 125 is", numOfInfectedYinB))
      print(paste("The total number of people infected in country X on day 125 is", numOfInfectedXinB))
      
}
summary(allD)

