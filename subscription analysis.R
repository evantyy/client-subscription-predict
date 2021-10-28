setwd("D:/et4_e")

#Import the bank marketing dataset
bank=read.table("bank.csv",sep=";",header=TRUE)

library(ggplot2)
library(dplyr)
library(gridExtra)

bank[sapply(bank, is.character)] <- lapply(bank[sapply(bank, is.character)], 
                                           as.factor)
dim(bank)

# Removing unknown values
bank1 = bank
bank1[bank1=="unknown"] <- NA
sum(is.na(bank1))
bank1 <- na.omit(bank1)


# Analysis of data with specific variables ---------------------------------------
# Checking if there are patterns between variables that affects Yes & No

ggplot(bank1, aes(x= age, fill = y))+ geom_boxplot(alpha = .3)+ggtitle("Age affecting subscription of term deposit")
## Age is only slightly significant for affecting our predicted value.

ggplot(bank1, aes(x=job, fill=y)) + geom_bar()+ggtitle("Jobs affecting subscription of term deposit")
## Jobs have a similar and even pattern between y, thus not a significant indicator.

ggplot(bank1, aes(x= marital, fill = y))+ geom_bar()+ggtitle("Marital status affecting subscription of term deposit")
## Insignificant difference in pattern between marital and y, not a significant indicator.

ggplot(bank1, aes(x=education, fill = y)) + geom_bar()+ggtitle("Education level affecting subscription of term deposit")
## Education have a similar even pattern, not a significant indicator.

ggplot(bank1, aes(x= duration, fill = y))+ geom_density(alpha = 0.2)+ggtitle("Duration affecting subscription of term deposit")
## Benchmarking Purpose: longer the call,  higher the chance of saying yes

ggplot(bank1, aes(x= campaign, fill = y)) +geom_bar() + xlim(0,20) + ggtitle("Subscribed term deposits by number of campaigns")
## Significant result, since if a person says yes, then they will probably do it within first 4 campaigns.


# Hold-out Validation method----------------------------------------------------
set.seed(123)
library(caTools)

sample = sample.split(bank1$y, SplitRatio = 0.7)
train = subset(bank1, sample==T)
test = subset(bank1, sample==F)

print(dim(train)); print(dim(test))
## train set with 70% data (535 observations, 17 variables) 
## test data with 30% data (229 observations, 17 variables).

prop.table(table(train$y))
prop.table(table(test$y))
## Baseline accuracy is 77 percent.



# Logistic Regression---------------------------------------------------------------------
bank_glm = glm(y ~ . , family="binomial", data = train)
summary(bank_glm)
##contactunknown, month of july, duration, poutcomesuccess are the more significant variables.

# Predicting with trainset
PredTrain = predict(bank_glm, data = train, type = "response")
# Confusion matrix
table(train$y, PredTrain >= 0.5)
(390+62)/nrow(train)  #Accuracy - 84%

# Predicting using testset
PredTest = predict(bank_glm, newdata = test, type = "response")
# Confusion matrix
table(test$y, PredTest >= 0.5)
(163+30)/nrow(test)  #Accuracy - 84%

## Baseline accuracy with 77%, both training and test set has 84 percent. 
## Overall, logistic regression model is better, and a good fit.


# Decision Tree using CART------------------------------------------------------------
set.seed(123)
library(rpart)
library(rpart.plot)
tree1 <- rpart(y~., data = train, method = 'class')
rpart.plot(tree1, extra = "auto")

pred_new <-predict(tree1, test, type="class")
table_1 <- table(test$y,pred_new)
table_1

accuracytest <- sum(diag(table_1))/sum(table_1)
print(paste('Test Accuracy', accuracytest))

accuracytuned <- function(tree1) {
  pred_new <- predict(tree1, test, type = 'class')
  table_1 <- table(test$y, pred_new)
  accuracytest <- sum(diag(table_1)) / sum(table_1)
  accuracytest
}

control <- rpart.control(minsplit = 4,
                         minbucket = round(4/ 3),
                         maxdepth = 9,
                         cp = 0)
tuned_mod <- rpart(y~., data = train, method = 'class', control = control)
accuracytuned(tuned_mod)
# Tuned parameters can lead to higher accuracy


# Random Forest-----------------------------------------------------------------
library(randomForest)
set.seed(123)

# making the models with 100 trees
subscribed_rf <- randomForest(y~., data=train, importance=T, ntree=100)

importance(subscribed_rf)
varImpPlot(subscribed_rf)
# Duration and month are important predictors on whether customers subscribe.

newtest <- predict(subscribed_rf,test)
error <- mean(newtest != test$y)
print(paste('Accuracy',1-error))
# The model is almost 84% accurate.
