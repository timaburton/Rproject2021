#Analysis Script 
source()
    #use to load functions in functions.R and compile into csv 

#use functions we defined 
#Answers to Questions w/ explanations in comments 
#figures to support Both Answers 

#?number of cases when and where? 
#?Vaccine data look at the end of the data as to what it is now? 
setwd("~/Desktop/Fall-2021/Biocomputing/")

allData <- read.table("allData.csv", stringsAsFactors = TRUE, header = TRUE, sep = ",")
head(allData)

positive <- NA 
for(i in 1:nrow(allData)){
  if(sum(allData[i,3:12])>=1){
    positive[(i+1)]<-1
  }else{
    positive[(i+1)]<-0
  }
}

positive <- positive[2:length(positive)]

allData <- cbind(allData, new_col = positive)

ggplot(data = allData, aes(x = dayofYear, y = new_col, fill=country))+
  geom_bar(stat='identity')+
  facet_grid(.~country)+
  ylab("Positive Cases")+
  xlab("Day of Year")+
  theme_classic()

# check 
#summary <- sum(allData$new_col)
#summary

marker1 <- sum(allData$marker01)
marker1
ggplot(data=allData, aes(x=as.numeric(),))+
  stat_sum(x=marker01)+
  facet_grid(.~country)+
  theme_classic()
  

for(i in 1:length(allData)){
  
}

allData1 <- rbind(allData, new_row = colSums(allData[,3:12]))
allData1
allData1[39880:39889,]


columns <- colSums(allData[,3:12])
columns

