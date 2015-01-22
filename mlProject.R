#set working directory first

library(caret)
library(ggplot2)

trainFileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testFileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("trainFile.csv")) download.file(trainFileUrl,"trainFile.csv",method="curl")
if(!file.exists("testFile.csv")) download.file(testFileUrl,"testFile.csv",method="curl")
if(!exists("trainDf")) trainDf<-read.table("trainFile.csv",header=TRUE,na.strings="",sep=",")
if(!exists("testDf")) testDf <- read.table("testFile.csv",header=TRUE,na.string="",sep=",")
#Data kindly provided by  http://groupware.les.inf.puc-rio.br/har

#Subset training set, since test set doesn't include classe variable
trainIndex<-sample(1:dim(trainDf)[1],size=dim(trainDf)[1]*.6,replace=F)
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
#And then, roll,pitch,yaw should be meaningful data processed from gyro and magnetometer, so remove extraneous
subTrainDf <- subTrainDf[,-grep("magnet",(names(subTrainDf)))]
subTrainDf <- subTrainDf[,-grep("gyro",names(subTrainDf))]
#Make sure numeric columns are actually numeric
ki <- (names(subTrainDf) != "classe") & (names(subTrainDf) != "new_window")
subTrainDf[,ki] <- data.frame(apply(subTrainDf[,ki],2,as.numeric))
subTestDf[,names(subTrainDf[,ki])] <-  data.frame(apply(subTestDf[,names(subTrainDf[,ki])],2,as.numeric))
#Check correlation of remaining numerical variables w/ classe
#keep variables whose correlation > 10% of the best predictor
predictorCor <- apply(subTrainDf[,ki],2,function(x) cor(as.numeric(subTrainDf$classe),x))
predictors <- predictorCor[predictorCor> .1*max(predictorCor)]
subTrainDf <- subTrainDf[,c(names(predictors),"classe","new_window")]

#modelFit <- train(classe~.,data=subTrainDf,method="rf")
#save(modelFit,file="modelFit.RData")
load(file="modelFit.RData")
#Validation
print(confusionMatrix(subTestDf$classe,predict(modelFit,newdata=subTestDf)))

#for kicks, check out what happens if you zero the strongest predictors
shitDf <- subTestDf
shitDf$pitch_forearm <- rep(0,nrow(shitDf))
shitDf$accel_arm_x <- rep(0,nrow(shitDf))
print(confusionMatrix(subTestDf$classe,predict(modelFit,newdata=shitDf)))
