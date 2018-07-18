## loading data
setwd("C:/Users/jl339/Desktop/datascience/project 2")
train_raw <- read.csv("train.csv")



## Cross validation
library(caret)
set.seed(200)
train <- createDataPartition(y=train_raw$Loan_Status,p=0.25,list=F)
training <- train_raw[train,]
testing <- train_raw[-train,]

## Cleaning the training data

trainingCl <- training[,2:13]
trainingCl [trainingCl ==""] <- NA
NArate <- apply(trainingCl, 2, function(x) sum(is.na(x)))/nrow(trainingCl)
trainingCl <- trainingCl[!(NArate>0.95)]
#trainingCl <- na.omit(trainingCl)

###PCA


preProc <- preProcess(trainingCl[1:52,],method="pca",thresh=.1) #12 components are required
preProc <- preProcess(trainingCl[1:52,],method="pca",thresh=.2) #18 components are required
preProc <- preProcess(trainingCl[1:52,],method="pca",thresh=.5) #25 components are required

preProc <- preProcess(trainingCl[1:52,],method="pca",pcaComp=25) 
preProc$rotation
trainingPC <- predict(preProc,trainingCl[1:52,])
  
  
## Random forest
library(randomForest)
modFitRF <- randomForest(trainingPC$Loan_Status~. ,  data=trainingPC, do.trace=T,na.action=na.exclude, ntree=50)
print(modFitRF) 

importance(modFitRF)

##Check with test set
testingCl <- testing[,2:13]
testingCl[testingCl==""] <- NA
NArate <- apply(testingCl, 2, function(x) sum(is.na(x)))/nrow(testingCl)
testingCl <- testingCl[!(NArate>0.95)]
testingPC <- predict(preProc,testingCl[1:52,])
#testingPC <-data.frame(testingPC $Married ,testingPC $Dependents ,testingPC $Property_Area,testingPC $Loan_Status)
confusionMatrix(testingPC$Loan_Status,predict(modFitRF,testingPC))

##Predict classes of test data
testdataCl <- read.csv("test.csv")
testdataPC <- predict(preProc,testdataCl)

levels(testdataPC$Married ) <- levels(trainingPC $Married )

testdataPC$Loan_Status <- predict(modFitRF,testdataPC)
confusionMatrix(testdataPC$Loan_Status,predict(modFitRF,testdataPC))
write.csv(testdataPC, "results.csv")
