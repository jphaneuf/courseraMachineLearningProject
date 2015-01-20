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

#Subset training set, since test set doesn't include classe variable
#Also remove timestamp/subject variables, don't want to apply those
trainIndex<-sample(1:dim(trainDf)[1],size=dim(trainDf)[1]*0.6,replace=F)
subTrainDf <- trainDf[trainIndex,-c(1,2,3,4,5,6,7)] #training df subset from original test set
subTestDf <- trainDf[-trainIndex,-c(1,2,3,4,5,6,7)] #testing df subset from original test set


#Impute Data, both training and test set.  Test set uses same preprocess as train set
numericColumns <- sapply(subTrainDf,is.numeric)
preProc <-preProcess(subTrainDf[,numericColumns],method="medianImpute")
subTrainDf[,numericColumns] <- predict(preProc,subTrainDf[,numericColumns])
subTestDf[,numericColumns] <- predict(preProc,subTestDf[,numericColumns])
testDf[,numericColumns] <- predict(preProc,testDf[,numericColumns])

#We're going to do a random forest on a fairly small sample, find the best predictors, then redo random forest on the best
#predictors on the full training set
testrf <- subTrainDf[1:1000,numericColumns]
testrf <- cbind(testrf,classe=subTrainDf$classe[1:1000])
modelFit <- train(classe~.,data=testrf,method="gbm")
x<-predict(modelFit,newdata=subTestDf)
###
modelFit2<- train(classe~ roll_dumbbell + gyros_dumbbell_y + magnet_dumbbell_y + magnet_dumbbell_z + pitch_forearm + roll_forearm + gyros_forearm_z
 + gyros_forearm_y + magnet_arm_z + gyros_arm_y + magnet_dumbbell_x + accel_dumbbell_y + pitch_dumbbell + accel_forearm_y
 + magnet_forearm_x + gyros_forearm_x + accel_forearm_x + magnet_arm_y + gyros_arm_x + magnet_forearm_y,
 data=subTrainDf,method="rf")
predictors <- subTrainDf[,c("roll_bumbbell","gyros_bumbell_y")]
fmodel <- randomForest(mtcars[,c("mpg","cyl")],mtcars$wt)

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

#varImp(modelFit)
# roll_forearm            1.5146
# magnet_belt_y           1.2339
# pitch_forearm           0.7240
# magnet_dumbbell_z       0.6236
# accel_forearm_x         0.5879
# magnet_arm_y            0.5010
# num_window              0.4869
# gyros_forearm_y         0.4620
# magnet_dumbbell_x       0.4456
# gyros_arm_z             0.3970
# magnet_dumbbell_y       0.3600
# yaw_forearm             0.3451
# roll_dumbbell           0.3421
# magnet_arm_z            0.3183
# accel_dumbbell_y        0.2876

# #
# roll_dumbbell      100.00
# gyros_dumbbell_y    96.67
# magnet_dumbbell_y   90.53
# magnet_dumbbell_z   85.58
# pitch_forearm       75.16
# roll_forearm        54.12
# gyros_forearm_z     51.85
# gyros_forearm_y     41.03
# magnet_arm_z        36.52
# gyros_arm_y         36.03
# magnet_dumbbell_x   36.02
# accel_dumbbell_y    34.69
# pitch_dumbbell      30.61
# accel_forearm_y     30.06
# magnet_forearm_x    29.90
# gyros_forearm_x     28.91
# accel_forearm_x     28.34
# magnet_arm_y        27.96
# gyros_arm_x         25.24
# magnet_forearm_y 