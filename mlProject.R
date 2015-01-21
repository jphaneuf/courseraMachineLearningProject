#set working directory first

library(caret)
library(ggplot2)
library(reshape2)

trainFileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testFileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("trainFile.csv")) download.file(trainFileUrl,"trainFile.csv",method="curl")
if(!file.exists("testFile.csv")) download.file(testFileUrl,"testFile.csv",method="curl")
if(!exists("trainDf")) trainDf<-read.table("trainFile.csv",header=TRUE,na.strings="",sep=",")
if(!exists("testDf")) testDf <- read.table("testFile.csv",header=TRUE,na.string="",sep=",")
#Data kindly provided by  http://groupware.les.inf.puc-rio.br/har

#Subset training set, since test set doesn't include classe variable
trainIndex<-sample(1:dim(trainDf)[1],size=dim(trainDf)[1]*0.6,replace=F)
subTrainDf <- trainDf[trainIndex,] #training df subset from original test set
subTestDf <- trainDf[-trainIndex,] #testing df subset from original test set

##Feature Selection
#view % NAs for each column
badIndex = vector()
#Remove columns who have more than 95% NAs.
for(i in 1:ncol(subTrainDf)){
  if(sum(is.na(subTrainDf[,i]))/nrow(subTrainDf) > .8){
    badIndex<-c(badIndex,i)
  } else if(sum(subTrainDf[,i]=="NA")/nrow(subTrainDf)>.8){
    badIndex<-c(badIndex,i)
  }
}
subTrainDf <- subTrainDf[,-badIndex]
#Remove user name,  timestamps, and 'X'.  X isn't described, but it appears to be some form of
#Timestamp, and on the training sets almost exactly predicts classe, so a bit of a red herring.
subTrainDf <- subTrainDf[,-c(1,2,3,4,5)]
#The magnetometer won't give us any additional information in this case:
subTrainDf <- subTrainDf[,-grep("magnet",(names(subTrainDf)))]

#We're going to do a random forest on a fairly small sample, find the best predictors, then redo random forest on the best
#predictors on the full training set
modelFit <- train(classe~.,data=subTrainDf,method="gbm")
x<-predict(modelFit,newdata=subTestDf)

##ok?
modelOutput <- predict(modelFit,newdata=testDf)
modelTester <- predict(modelFit2,newdata=subTestDf)
confusionMatrix(testing$type,predict(modelFit,testPC))


#Non-linear data -> Random Forest

#Build Random Forest Model
#Does the submission build a machine learning algorithm to predict activity quality from activity monitors?


#Do the authors describe what they expect the out of sample error to be and estimate 
#the error appropriately with cross-validation?

#Github pages

