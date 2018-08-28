####Loading the libraries
library(readxl)
library(dplyr)
library(xlsx)
require(XLConnect) 
library(DMwR)
library(infotheo)
library(dummies) 
library(vegan)
library(caret)
library(rpart)
library(MASS)
library(ggplot2)
library(pROC)
library(corrplot)

##Importing the train data
rawdata <- read.csv("Train.csv")

data <- rawdata
sum(is.na(data))

##Converting the values of -9 and empty values into NA's
data[data=="" | data==-9] <- NA

str(data)
summary(data)
sum(is.na(data))

######Converting the variables in to categorical form
data[c("gender","chestpainloc","painexertion","reliefbyrest","chestpaintype","IsSmoker",
     "FastingBS","FamHistory","restingECGRes","ECGspecsDigitalis","ECGspecsBetaBlocker","ECGspecsNitrates",
     "ECGspecsCalci","ECGspecsdiuretic","ExInducedAngina","xhypo","Slope","restwallmotion","ExerWallmotion",
     "thalassemia","Severity","LeftMaintrunk","ladproximation","laddistance","cxmain","ramus",
     "obtuseMarginal1","obtuseMarginal2","rcaproximation","rcadistance","nameOfPatient","Location")
     ] <- lapply(data[c("gender","chestpainloc","painexertion","reliefbyrest","chestpaintype","IsSmoker",
                    "FastingBS","FamHistory","restingECGRes","ECGspecsDigitalis","ECGspecsBetaBlocker","ECGspecsNitrates",
                    "ECGspecsCalci","ECGspecsdiuretic","ExInducedAngina","xhypo","Slope","restwallmotion","ExerWallmotion",
                    "thalassemia","Severity","LeftMaintrunk","ladproximation","laddistance","cxmain","ramus",
                    "obtuseMarginal1","obtuseMarginal2","rcaproximation","rcadistance","nameOfPatient","Location")
                    ] , factor)


# delete rows with more than 30% percent missing
rowmis <- c()
for(i in 1:nrow(data)) {
  if(length(which(is.na(data[i,]))) > 0.30*ncol(data)) rowmis <-######imputing missing values
append(rowmis,i) 
}
data <- data[-rowmis,]

library(missForest)
imputationResults <- missForest(data)
dataMissForestImputed <- imputationResults$ximp
data=dataMissForestImputed
str(data)
summary(data)
sum(is.na(data)) 
str(data)
summary(data)

sum(is.na(data$id))



nwdata <- data[-c(1,60,46,42)]
str(nwdata)


##For correlation plot
data_numeric <- data[c("age","restingBP","serumcholesterol","NumCigs","SmokingPeriod",
                     "MonthECG","DayECG","YearECG","ExTestDuration","TimeSTNoted","Metabolism",
                     "MaxHR","RestHRGen","PeakExBP1","PeakExBP2","restingBPGen","Stdepression","HeightatRest","heightatPeakEx","NumMajVessels","restRaidonuclid","exerRadinalid","thalsev","thalpul","earlobe",
"coronaryAngioMonth","coronaryAngioDay","coronaryAngioYear")]

par(mfrow = c(1,1))
cor_matrix=cor(data_numeric)
corrplot(cor_matrix, method = "circle")


str(data)
###Using the variable importance in RF model reduced the variables to 33.
data2 <- data[c(1,6,7,8,11,57,53,52,55,51,50,49,58,33,27,32,9,54,60,26,28,46,23,17,16,31,37,29,30,36,47,25,15,24)]
str(data2)

#######Finding the outliers in the dataset

hist(data$restingBP)
hist(data$serumcholesterol)
hist(data$Metabolism)

boxplot(data$restingBP)

dt <- log(data$Metabolism)
hist(dt)

hist(log(data2$Metabolism))

######Scaling the data
scale1 <- preProcess(data2,method = "scale")
scaleddata <- predict(scale1,data2)

###Visualizations
library(ggplot2)
par(mfrow=c(1,2)) # Customize the params; mfrow=c(1,2) specifies 2 multiple figures in 1 row;

hist(data$Metabolism, col="green", xlab="Metabolism")

boxplot(data$Metabolism)

par(mfrow=c(1,1))

plot(data$age,data$NumCigs)

ggplot(data = data,aes(x=data$NumCigs,fill=data$age)) + 
  geom_bar()

ggplot(data=data, aes(x=data$Severity, y=data$Metabolism))+geom_point()

ggplot(data=data, aes(x=data$Severity, y=data$MaxHR))+geom_point()
                           

# Learning Curve for Svm
set.seed(1)
cdataIndex <- createDataPartition(nwdata$Severity, p = .85, list = F)
cdataTrain <- nwdata[cdataIndex,]
cdataTest <- nwdata[-cdataIndex,]

# create empty data frame 

nsteps = 20
learnCurve <- data.frame(datasamples = integer(nsteps),
                         trainMetric = integer(nsteps),
                         testMetric = integer(nsteps))

# test data response feature
testY <- cdataTest$Severity

# Run algorithms using 5-fold cross validation with 1 repeats
trainControl <- trainControl(method="repeatedcv", number=3, repeats=1)
metric <- "Accuracy"

nrows <- nrow(cdataTrain)
# loop over training examples
for (j in 1:nsteps*floor(nrows/nsteps)) {
  i = j/floor(nrows/nsteps)
  cat(i)
  learnCurve$datasamples[i] <- j
  
  # train learning algorithm with size i
  sampledData <- cdataTrain[sample(j),]
  fit.svm <- train(Severity~., 
                   data=sampledData, 
                   method="svmLinear", 
                   metric=metric,
                   # preProc=c("center", "scale"), 
                   trControl=trainControl)        
  
  prediction <- predict(fit.svm, newdata = sampledData)
  
  Metric <- postResample(prediction, sampledData$Severity)
  
  learnCurve$trainMetric[i] <- Metric[1]
  # use trained parameters to predict on test data
  prediction <- predict(fit.svm, newdata = cdataTest)
  Metric <- postResample(prediction, testY)
  learnCurve$testMetric[i] <- Metric[1]
}

library(ggplot2)
p <- ggplot(learnCurve)
p <- p + aes(x = datasamples) + 
  geom_line(aes(y = trainMetric, colour='train Metric')) + 
  geom_line(aes(y = testMetric, colour='test Metric'))
ggsave(filename=paste(1, "myPlot.jpg"), plot=p)


}

# Learning Curve for RF
set.seed(1)
cdataIndex <- createDataPartition(nwdata$Severity, p = .85, list = F)
cdataTrain <- nwdata[cdataIndex,]
cdataTest <- nwdata[-cdataIndex,]

# create empty data frame 

nsteps = 20
learnCurve <- data.frame(datasamples = integer(nsteps),
                         trainMetric = integer(nsteps),
                         testMetric = integer(nsteps))

# test data response feature
testY <- cdataTest$Severity

# Run algorithms using 5-fold cross validation with 1 repeats
trainControl <- trainControl(method="repeatedcv", number=3, repeats=1)
metric <- "Accuracy"

nrows <- nrow(cdataTrain)
# loop over training examples
for (j in 1:nsteps*floor(nrows/nsteps)) {
  i = j/floor(nrows/nsteps)
  cat(i)
  learnCurve$datasamples[i] <- j
  
  # train learning algorithm with size i
  sampledData <- cdataTrain[sample(j),]
  fit.rf <- train(Severity~., 
                   data=sampledData, 
                   method="rf", 
                   metric=metric,
                   # preProc=c("center", "scale"), 
                   trControl=trainControl)        
  
  prediction <- predict(fit.rf, newdata = sampledData)
  
  Metric <- postResample(prediction, sampledData$Severity)
  
  learnCurve$trainMetric[i] <- Metric[1]
  # use trained parameters to predict on test data
  prediction <- predict(fit.rf, newdata = cdataTest)
  Metric <- postResample(prediction, testY)
  learnCurve$testMetric[i] <- Metric[1]
}

library(ggplot2)
p <- ggplot(learnCurve)
p <- p + aes(x = datasamples) + 
  geom_line(aes(y = trainMetric, colour='train Metric')) + 
  geom_line(aes(y = testMetric, colour='test Metric'))
ggsave(filename=paste(2, "myPlot.jpg"), plot=p)


}

######Scaling the data
scale1 <- preProcess(nwdata,method = "scale")
scaleddata <- predict(scale1,nwdata)

######train and validate
set.seed(123)
train_rows <- sample(x = 1:nrow(scaleddata), size = 0.7*nrow(scaleddata))
train<- scaleddata[train_rows, ]
dim(train)
valid<- scaleddata[-train_rows, ]
dim(valid)

######Building the Random Forest Model
library(randomForest)
heart_rf<- randomForest(Severity ~ ., data=train, keep.forest=TRUE, ntree=500,mtry=29)
summary(heart_rf)

#Predicting on train data
pred1<-predict(heart_rf,train);pred1

#Predicting on valid data
pred2<-predict(heart_rf,valid);pred2

#Confusion Matrix
confusionMatrix(pred1,train$Severity)
confusionMatrix(pred2,valid$Severity)

print(heart_rf)
heart_rf$importance

varImpPlot(heart_rf) 

conf <- heart_rf$confusion
conf
  

#####Decision Tree
library(rpart)
decision_tree_model <- rpart(Severity ~ .,train, method = "class")
decision_tree_model
library(rpart.plot)
rpart.plot(decision_tree_model, type = 4, extra = 101) 

#Predictions on test data
p <- predict(decision_tree_model,valid, type = "class")
tab<-table(valid$Severity, p)

tab
dim(valid)

#######SVM
install.packages("e1071")
library(e1071)
model=train(Severity~., data=train, method='svmLinear')
#grid
grid=expand.grid(C=10^c(-3:3))
ctrl=trainControl(method='cv',number=5)

linear.svm.tune=train(Severity~.,
                      data=train,
                      method='svmLinear',
                      metric='Accuracy',
                      tuneGrid=grid,
                      trControl=ctrl)
linear.svm.tune
linear.svm.tune$results

ctrl=trainControl(method='cv',
                  number=5,
                  search = 'random')

radial.svm.tune=train(Severity~.,data=train,
                      method='svmLinear',
                      metric='Accuracy',
                      tuneLength=50,
                      trControl=ctrl)
test_pred=predict(linear.svm.tune,cdataTest1)
confusionMatrix(test_pred, cdataTest1$Severity )



######Scaling the data
scale1 <- preProcess(nwdata,method = "scale")
scaleddata <- predict(scale1,nwdata)

######train and validate
set.seed(123)
train_rows <- sample(x = 1:nrow(scaleddata), size = 0.7*nrow(scaleddata))
train<- scaleddata[train_rows, ]
dim(train)
valid<- scaleddata[-train_rows, ]
dim(valid)
###C50
#Decision Trees using C5.0 (For Classification Problem)
#Loading library for C5.0
library(C50)
#calling C5.0 function
dtC50= C5.0(Severity ~ ., data = train, rules=TRUE)
#dtc50 = C5.0(loan~., data=train, control = C5.0Control(minCases=5,))
summary(dtC50)
C5imp(dtC50, pct=TRUE)

#Model Evaluation- Error Metrics 
a=table(train$Severity, predict(dtC50, newdata=train, type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100; rcTrain
a=table(valid$Severity, predict(dtC50, newdata=valid, type="class"))
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100; rcTest



####Test data

testdata <- read.csv("Test.csv")

testdata[c("gender","chestpainloc","painexertion","reliefbyrest","chestpaintype","IsSmoker",
       "FastingBS","FamHistory","restingECGRes","ECGspecsDigitalis","ECGspecsBetaBlocker","ECGspecsNitrates",
       "ECGspecsCalci","ECGspecsdiuretic","ExInducedAngina","xhypo","Slope","restwallmotion","ExerWallmotion",
       "thalassemia","LeftMaintrunk","ladproximation","laddistance","cxmain","ramus",
       "obtuseMarginal1","obtuseMarginal2","rcaproximation","rcadistance","nameOfPatient","Location")
     ] <- lapply(testdata[c("gender","chestpainloc","painexertion","reliefbyrest","chestpaintype","IsSmoker",
                        "FastingBS","FamHistory","restingECGRes","ECGspecsDigitalis","ECGspecsBetaBlocker","ECGspecsNitrates",
                        "ECGspecsCalci","ECGspecsdiuretic","ExInducedAngina","xhypo","Slope","restwallmotion","ExerWallmotion",
                        "thalassemia","LeftMaintrunk","ladproximation","laddistance","cxmain","ramus",
                        "obtuseMarginal1","obtuseMarginal2","rcaproximation","rcadistance","nameOfPatient","Location")
                      ] , factor)

testdata[testdata=="" | testdata==-9] <- NA

######imputing missing values
library(missForest)
imputationResults <- missForest(testdata)
dataMissForestImputed <- imputationResults$ximp
testdata=dataMissForestImputed
str(testdata)
summary(testdata)
sum(is.na(testdata))

categ.data.test <- testdata
categ.data <- train
# Now setting the same set of factors for both test and train
categ.data.test$ECGspecsBetaBlocker <- as.character((categ.data.test$ECGspecsBetaBlocker))
categ.data.test$thalassemia <- as.character(categ.data.test$thalassemia)
categ.data.test$LeftMaintrunk <- as.character(categ.data.test$LeftMaintrunk)
categ.data.test$Slope <- as.character((categ.data.test$Slope))
#categ.data.test$ExerWallmotion <- as.character(categ.data.test$ExerWallmotion)

categ.data$ECGspecsBetaBlocker <- as.character((categ.data$ECGspecsBetaBlocker))
categ.data$thalassemia <- as.character(categ.data$thalassemia)
categ.data$LeftMaintrunk <- as.character(categ.data$LeftMaintrunk)
categ.data$Slope <- as.character((categ.data$Slope))
categ.data$ExerWallmotion <- as.character(categ.data$ExerWallmotion)






newLevels <- setdiff(unique(c(categ.data$ECGspecsBetaBlocker,categ.data.test$ECGspecsBetaBlocker)),NA)
categ.data$ECGspecsBetaBlocker <- factor(categ.data$ECGspecsBetaBlocker, levels = newLevels)
categ.data.test$ECGspecsBetaBlocker <- factor(categ.data.test$ECGspecsBetaBlocker, levels = newLevels)

newLevels <- setdiff(unique(c(categ.data$LeftMaintrunk,categ.data.test$LeftMaintrunk)),NA)
categ.data$LeftMaintrunk <- factor(categ.data$LeftMaintrunk, levels = newLevels)
categ.data.test$LeftMaintrunk <- factor(categ.data.test$LeftMaintrunk, levels = newLevels)

newLevels <- setdiff(unique(c(categ.data$Slope,categ.data.test$Slope)),NA)
categ.data$Slope <- factor(categ.data$Slope, levels = newLevels)
categ.data.test$Slope <- factor(categ.data.test$Slope, levels = newLevels)

newLevels <- setdiff(unique(c(categ.data$thalassemia,categ.data.test$thalassemia)),NA)
categ.data$thalassemia <- factor(categ.data$thalassemia, levels = newLevels)
categ.data.test$thalassemia <- factor(categ.data.test$thalassemia, levels = newLevels)

newLevels <- setdiff(unique(c(categ.data$ExerWallmotion,categ.data.test$ExerWallmotion)),NA)
categ.data$ExerWallmotion <- factor(categ.data$ExerWallmotion, levels = newLevels)
categ.data.test$ExerWallmotion <- factor(categ.data.test$ExerWallmotion, levels = newLevels)



testdata <- testdata[-c(1,59,46,42)]
testdata$Severity <- NA

newLevels

predict_test <- predict(heart_rf, testdata)
levels(testdata$ECGspecsBetaBlocker) <- levels(train$ECGspecsBetaBlocker)
  levels(testdata$thalassemia) <- levels(train$thalassemia)
levels(testdata$LeftMaintrunk) <-
  levels(train$Slope)
train$exerwallmotion
"gender","chestpainloc","painexertion","reliefbyrest","chestpaintype","IsSmoker",
"FastingBS","FamHistory","restingECGRes","ECGspecsDigitalis","ECGspecsBetaBlocker","ECGspecsNitrates",
"ECGspecsCalci","ECGspecsdiuretic","ExInducedAngina","xhypo","Slope","restwallmotion","ExerWallmotion",
"thalassemia","LeftMaintrunk","ladproximation","laddistance","cxmain","ramus",
"obtuseMarginal1","obtuseMarginal2","rcaproximation","rcadistance","nameOfPatient","Location"
str(testdata)



predict_test

predictions <- data.frame(test$id,predict_test)
str(predictions)
colnames(predictions) <- c("id","Severe")

write.csv(predictions,"predictions.csv")

data1=read.csv("Train.csv")
data2=read.csv("Test.csv")
data3=rbind(data1,data2)
colnames(data3)

