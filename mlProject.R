#set working directory first

library(caret)

trainFileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testFileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("trainFile.csv")) download.file(trainFileUrl,"trainFile.csv",method="curl")
if(!file.exists("testFile.csv")) download.file(testFileUrl,"testFile.csv",method="curl")
if(!exists("trainDf")) trainDf<-read.csv("trainFile.csv")
if(!exists("testDf")) testDf <- read.csv("testFile.csv")
#Data kindly provided by  http://groupware.les.inf.puc-rio.br/har


#Explore data to find predictors
#RF1
#Subset for testing
trainIndex<-sample(1:dim(trainDf)[1],size=dim(trainDf)[1]/200,replace=F)
testtestDf <- trainDf[-trainIndex,]
testtrainDf <- trainDf[trainIndex,]

#PCA
numericColumns <- sapply(trainDf,is.numeric)
trainForPCA <- trainDf[numericColumns]
preProc <- preProcess(trainForPCA,method="pca",pcaComp=6) #build PCA object
trainPC <- predict(preProc,trainForPCA) #apply PCA object to new dataFrame
modelFit <- train(trainDf$classe ~ .,method="rf",data=trainPC)

##ok?
testForPCA <- testtestDf[numericColumns]
testPC <- predict(preProc,testForPCA)
modelOutput <- predict(modelFit,newdata=testPC)
confusionMatrix(testing$type,predict(modelFit,testPC))


#Non-linear data -> Random Forest

#Build Random Forest Model
#Does the submission build a machine learning algorithm to predict activity quality from activity monitors?


#Do the authors describe what they expect the out of sample error to be and estimate 
#the error appropriately with cross-validation?

#Github pages