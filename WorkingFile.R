library(tidyverse)
library(readr)
library(dplyr)
library(knitr)
library(kableExtra)
library(forcats)
library(Envstats)
library(earth)
library(outliers)
library(dplyr)
library(MASS)
library(lars)
library(e1071)
library(mice)
library(naniar)
library(pls)
library(lars)
library(rpart)          

test <- read.csv("hm7-Test.csv")
train <- read.csv("hm7-Train.csv")
attach(train)


train %>% select_if(is.numeric) %>% names()
train %>% select_if(is.factor) %>% names()
train %>% select_if(is.character) %>% names()

train$readmitted <- as.factor(train$readmitted)

#Create DT Model
fitDT<-rpart(readmitted~ num_lab_procedures + num_medications 
             + race + admission_type,data=train)  

#Visualize what the tree is doing
prp(fitDT, type=2, extra=104, nn=TRUE, fallen.leaves=TRUE,
    faclen=0,varlen=0, shadow.col="grey", branch.lty=3)

#Get the data frame of predictions. This is a probability representation
predictions <- predict(fitDT, newdata=test)

#Loop though each of the probabilities and assign the
#predicted value based on the most likely choice
vec <- vector()
for (row in 1:nrow(predictions))
{
  if (predictions[row, "0"] >= 0.5)
  {
      vec <- c(vec, 0)
  }
  else
  {
     vec <- c(vec, 1)
  }
}

#Convert To data Frame
predictDataFrame <- data.frame(vec)

#Add the Patient IDs
predictDataFrame$patientID <- test$patientID

#Swap column order
predictDataFrame <- predictDataFrame[, c(2, 1)]

#rename the columns
colnames(predictDataFrame) <- c("patientID", "readmitted")

#export to csv
write.csv(predictDataFrame, "submission.csv", row.names=FALSE)




