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

train <- trainOrg
test <- testOrg

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

train$Gender <- as.character(train$Gender)
train$Gender[train$Gender == 'M'] <- 1
train$Gender[train$Gender == 'F'] <- 2
train$Gender <- as.factor(train$Gender)

test$Gender <- as.character(test$Gender)
test$Gender[test$Gender == 'M'] <- 1
test$Gender[test$Gender == 'F'] <- 2
test$Gender <- as.factor(test$Gender)

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
train$Occupation <- as.factor(train$Occupation)
test$Occupation <- as.factor(test$Occupation)

#City_Category
ggplot(data = train, aes(x = City_Category)) + geom_bar()
ggplot(data = test, aes(x = City_Category)) + geom_bar()
ggplot(data = train, aes(x = City_Category)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = City_Category), fill = "blue")

#Stay_In_Current_City_Years
ggplot(data = train, aes(x = Stay_In_Current_City_Years)) + geom_bar()
ggplot(data = test, aes(x = Stay_In_Current_City_Years)) + geom_bar()
ggplot(data = train, aes(x = Stay_In_Current_City_Years)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Stay_In_Current_City_Years), fill = "blue")


#Marital_Status
str(train$Marital_Status)
train$Marital_Status <- as.factor(train$Marital_Status)
test$Marital_Status <- as.factor(test$Marital_Status)

ggplot(data = train, aes(x = Marital_Status)) + geom_bar()
ggplot(data = test, aes(x = Marital_Status)) + geom_bar()
ggplot(data = train, aes(x = Marital_Status)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Marital_Status), fill = "blue")

#Product_Category_1

ggplot(data = train, aes(x = Product_Category_1)) + geom_bar()
ggplot(data = test, aes(x = Product_1)) + geom_bar()
ggplot(data = train, aes(x = Product_1)) + geom_bar(fill = "yellow") + geom_bar(data = test, aes(x = Product_1), fill = "blue")
train$Product_Category_1 <- as.factor(train$Product_Category_1)
test$Product_Category_1 <- as.factor(test$Product_Category_1)

str(train)
#Product_Category_2
ggplot(data = train, aes(x = Product_Category_2)) + geom_bar()
str(train$Product_Category_2)
table(train$Product_Category_2)

#Predict Missing Values
#randomforest
library(party)
library(randomForest)
library(plyr)

full <- join(test, train, type = "full")
full$Product_Category_1 <- as.factor(full$Product_Category_1)
full$Product_Category_2 <- as.factor(full$Product_Category_2)
full$Product_Category_3 <- as.factor(full$Product_Category_3)


# pc1 <- lm(Product_Category_2 ~ Gender +  Age + City_Category 
#                + Stay_In_Current_City_Years + Marital_Status 
#                + Purchase, data = full)
# 
# train$Product_Category_2[is.na(train$Product_Category_2)] <- predict(pc1, train)[is.na(train$Product_Category_2)]
# test$Product_Category_2[is.na(test$Product_Category_2)] <- predict(pc1, test)[is.na(test$Product_Category_2)]

#Decision tree
library(rpart)
pc1 <-rpart(Product_Category_2 ~ Gender +  Age + City_Category 
            + Stay_In_Current_City_Years + Marital_Status 
            + Product_Category_1 , data = full)
train$Product_Category_2[is.na(train$Product_Category_2)] <- predict(pc1, train)[is.na(train$Product_Category_2)]
test$Product_Category_2[is.na(test$Product_Category_2)] <- predict(pc1, test)[is.na(test$Product_Category_2)]

pc2 <-rpart(Product_Category_3 ~ Gender +  Age + City_Category 
            + Stay_In_Current_City_Years + Marital_Status 
            + Product_Category_1 , data = full)
train$Product_Category_3[is.na(train$Product_Category_3)] <- predict(pc2, train)[is.na(train$Product_Category_3)]
test$Product_Category_3[is.na(test$Product_Category_3)] <- predict(pc2, test)[is.na(test$Product_Category_3)]

modelData <- train[,c(3:8,12:15)]
newData <- test[,c(3:8,12:14)]

#Regression Model
regData <- train[,c(3:12)]
regTestData <- test[,c(3:11)]

regModel <- lm( Purchase ~ ., data = regData)
anova(regModel)
prdict <- predict(regModel, regTestData)
write.csv(prdict, "Final_Submission_Regression4.csv")

#Decision tree
decisionModel <-rpart(Purchase ~ ., data=regData)
decisionPredict <- predict(decisionModel, regTestData)
write.csv(decisionPredict, file = 'Final_Submission_Decision_Tree2.csv', row.names = FALSE)

#randomforest
library(party)
library(randomForest)

forestmodel <- randomForest(Purchase ~ Gender + Age + Occupation + City_Category
                            + Stay_In_Current_City_Years + Marital_Status
                            + Product_Category_1  + Product_Category_2
                            + Product_Category_3, data = regData, ntree = 25)

prediction <- predict(forestmodel, newdata = regTestData)

write.csv(prediction, file = 'Final_Submission_RandomForest.csv', row.names = FALSE)

library(foreach)
library(doParallel)

#setup parallel back end to use 8 processors
cl<-makeCluster(12)
registerDoParallel(cl)

# divide row size by 20, sample data 400 times 
length_divisor <- 200000
predictions<-foreach(m=1:10,.combine=cbind) %dopar% { 
  # using sample function without seed
  sampleRows <- sample(nrow(regData), size=floor((nrow(regData)/length_divisor)))
  fit <- lm(Purchase ~ ., data = regData)
  predictions <- data.frame(predict(object=fit, regTestData, se.fit = TRUE)[[1]])
} 
stopCluster(cl)
write.csv(predictions, "Final_Submission_Bagging4.csv")

library(pROC)
auc(testdf[,outcomeName], rowMeans(predictions))
