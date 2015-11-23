#' ---------------------------------------------------------------
#' @version Black Friday Hackathan
#' @title Product Purchase
#' 
#' @description 
#' This script is used to analyze the AnalyticsVidhya black friday competition data. 
#' 
#' @author Vijayan Nagarajan
#' ---------------------------------------------------------------


#set working directory
setwd("D:\\AnalyticsVidhya\\Black Friday Hackathan\\")

#load data
trainOrg <- read.csv(".\\train.csv")
testOrg <- read.csv(".\\test.csv")

train <- na.omit(trainOrg)
test <- na.omit(testOrg)

#check the data
str(train)
str(test)

#check for missing values (if any)
table(is.na(train))

#Feature Engineering

#Use plot to check level names and their frequency, done seperately for train and test due to difference in level names
#Gender
library(ggplot2)
ggplot(data = train, aes(x = Gender)) + geom_bar()
ggplot(data = test, aes(x = Gender)) + geom_bar()

#Age
ggplot(data = train, aes(x = Age)) + geom_bar()
ggplot(data = test, aes(x = Age)) + geom_bar()
ggplot(data = train, aes(x = Age)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Age), fill = "blue")

levels(train$Age)[levels(train$Age) == '0-17'] <- '0-25'
train$Age[train$Age == '18-25'] <- "0-25"
levels(train$Age)[levels(train$Age) == '26-35'] <- '26-45'
train$Age[train$Age == '36-45'] <- "26-45"
levels(train$Age)[levels(train$Age) == '46-50'] <- '46+'
train$Age[train$Age == '51-55'] <- "46+"
train$Age[train$Age == '55+'] <- "46+"
table(train$Age)

train$Age <- droplevels(train$Age)

levels(test$Age)[levels(test$Age) == '0-17'] <- '0-25'
test$Age[test$Age == '18-25'] <- "0-25"
levels(test$Age)[levels(test$Age) == '26-35'] <- '26-45'
test$Age[test$Age == '36-45'] <- "26-45"
levels(test$Age)[levels(test$Age) == '46-50'] <- '46+'
test$Age[test$Age == '51-55'] <- "46+"
test$Age[test$Age == '55+'] <- "46+"
table(test$Age)

test$Age <- droplevels(test$Age)

ggplot(data = train, aes(x = Age)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Age), fill = "blue")

#Occupation
ggplot(data = train, aes(x = Occupation)) + geom_bar()
ggplot(data = test, aes(x = Occupation)) + geom_bar()
ggplot(data = train, aes(x = Occupation)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Occupation), fill = "blue")

train$Occupation[train$Occupation < 8] <- 1
train$Occupation[train$Occupation > 7] <- 0
table(train$Occupation)
train$Occupation <- as.factor(train$Occupation)
levels(train$Occupation)[levels(train$Occupation) == '0'] <- 'Low'
levels(train$Occupation)[levels(train$Occupation) == '1'] <- 'High'
levels(train$Occupation)

train$Occupation <- droplevels(train$Occupation)

test$Occupation[test$Occupation < 8] <- 1
test$Occupation[test$Occupation > 7] <- 0
table(test$Occupation)
test$Occupation <- as.factor(test$Occupation)
levels(test$Occupation)[levels(test$Occupation) == '0'] <- 'Low'
levels(test$Occupation)[levels(test$Occupation) == '1'] <- 'High'
levels(test$Occupation)

train$Occupation <- droplevels(train$Occupation)

#City_Category
ggplot(data = train, aes(x = City_Category)) + geom_bar()
ggplot(data = test, aes(x = City_Category)) + geom_bar()
ggplot(data = train, aes(x = City_Category)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = City_Category), fill = "blue")

#Stay_In_Current_City_Years
ggplot(data = train, aes(x = Stay_In_Current_City_Years)) + geom_bar()
ggplot(data = test, aes(x = Stay_In_Current_City_Years)) + geom_bar()
ggplot(data = train, aes(x = Stay_In_Current_City_Years)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Stay_In_Current_City_Years), fill = "blue")

train$Stay_In_Current_City_Years[train$Stay_In_Current_City_Years == '0'] <- '1'
train$Stay_In_Current_City_Years[train$Stay_In_Current_City_Years == '3'] <- '2'

train$Stay_In_Current_City_Years <- droplevels(train$Stay_In_Current_City_Years)
table(train$Stay_In_Current_City_Years)

test$Stay_In_Current_City_Years[test$Stay_In_Current_City_Years == '0'] <- '1'
test$Stay_In_Current_City_Years[test$Stay_In_Current_City_Years == '3'] <- '2'

test$Stay_In_Current_City_Years <- droplevels(test$Stay_In_Current_City_Years)
table(test$Stay_In_Current_City_Years)

#Marital_Status
str(train$Marital_Status)
train$Marital_Status <- as.factor(train$Marital_Status)
test$Marital_Status <- as.factor(test$Marital_Status)

ggplot(data = train, aes(x = Marital_Status)) + geom_bar()
ggplot(data = test, aes(x = Marital_Status)) + geom_bar()
ggplot(data = train, aes(x = Marital_Status)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Marital_Status), fill = "blue")

#Product_Category_1
str(train$Product_Category_1)
#train$Product_Category_1 <- trainOrg$Product_Category_1
train$Product_1 <- 2
train$Product_1[train$Product_Category_1 < 5] <- 1
#train$Product_Category_1[train$Product_Category_1 > 4 && train$Product_Category_1 < 8] <- 2
train$Product_1[train$Product_Category_1 > 8] <- 3
table(train$Product_1)
train$Product_1 <- as.factor(train$Product_1)

test$Product_1 <- 2
test$Product_1[test$Product_Category_1 < 5] <- 1
#train$Product_Category_1[train$Product_Category_1 > 4 && train$Product_Category_1 < 8] <- 2
test$Product_1[test$Product_Category_1 > 8] <- 3
table(test$Product_1)
test$Product_1 <- as.factor(test$Product_1)

ggplot(data = train, aes(x = Product_1)) + geom_bar()
ggplot(data = test, aes(x = Product_1)) + geom_bar()
ggplot(data = train, aes(x = Product_1)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Product_1), fill = "blue")

str(train)
#Product_Category_2
ggplot(data = train, aes(x = Product_Category_2)) + geom_bar()
str(train$Product_Category_2)
table(train$Product_Category_2)

# 1 - around 50k - 2,8,14,15,16; 2 - around 25k - 4,5,6,11,13,17; rest - 3
train$Product_2 <- 1
train$Product_2[train$Product_Category_2 > 8] <- 2
train$Product_2[train$Product_Category_2 > 13] <- 3
table(train$Product_2)
train$Product_2 <- as.factor(train$Product_2)


test$Product_2 <- 1
test$Product_2[test$Product_Category_2 > 8] <- 2
test$Product_2[test$Product_Category_2 > 13] <- 3
table(test$Product_2)
test$Product_2 <- as.factor(test$Product_2)

ggplot(data = train, aes(x = Product_2)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Product_2), fill = "blue")


#Product_Category_3
ggplot(data = train, aes(x = Product_Category_3)) + geom_bar()
ggplot(data = test, aes(x = Product_Category_3)) + geom_bar()

train$Product_3 <- 1
train$Product_3[train$Product_Category_3 > 11] <- 2
table(train$Product_3)
train$Product_3 <- as.factor(train$Product_3)


test$Product_3 <- 1
test$Product_3[test$Product_Category_3 > 11] <- 2
table(test$Product_3)
test$Product_3 <- as.factor(test$Product_3)

ggplot(data = train, aes(x = Product_3)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Product_3), fill = "blue")

modelData <- train[,c(3:8,12:15)]
newData <- test[,c(3:8,12:14)]

#Regression Model
regData <- train[,c(3:12)]
regData[is.na(regData)] <- 0
regTestData <- test[,c(3:11)]
regTestData[is.na(regTestData)] <- 0

regModel <- lm( Purchase ~ ., data = regData)
anova(regModel)
prdict <- predict(regModel, regTestData)
output <- as.data.frame('')
output$User_ID <- test$User_ID
output$Product_ID <- test$Product_ID
output <- c(test[,1:2], prdict[,1])
write.csv(prdict, "Final_Submission_Regression3.csv")


#randomforest
library(party)
library(randomForest)

forestmodel <- randomForest(Purchase ~ Gender +  City_Category
                           + Product_Category_1  + Product_Category_2
                            + Product_Category_3, data = regData)

prediction <- predict(forestmodel, newdata = regTestData)

write.csv(prediction, file = 'Final_Submission_RandomForest.csv', row.names = FALSE)

#Decision tree
library(rpart)
decisionModel <-rpart(Purchase ~ ., data=modelData)
decisionPredict <- predict(decisionModel, newData)
write.csv(decisionPredict, file = 'Final_Submission_Decision_Tree.csv', row.names = FALSE)

#SVM
install.packages('e1071')
library(e1071)
svmModel <- svm(Purchase ~ ., data=modelData)
svmPredicted <- predict(svmModel, newData)
write.csv(decisionPredict, file = 'Final_Submission_SVM.csv', row.names = FALSE)

#NaiveBayes
nbModel <- naiveBayes(Purchase ~ ., data=modelData)
nbPredicted <- predict(nbModel, newData)
write.csv(decisionPredict, file = 'Final_Submission_NB.csv', row.names = FALSE)

#Neural Networks
install.packages('neuralnet')
library("neuralnet")
nnModel <- neuralnet(Purchase ~ ., modelData, hidden=10, threshold=0.01)
