##IS507 Assignment 7
##ChengHsuan Lin / 11212021

#Setup Working Directory
setwd("~/Downloads/02_IS 507/W13 Linear Discriminant Analysis:Logistic regression/Assignment 7")

#Read in Data
library(readr)
LDA <- read_csv("Employee.csv", col_names = TRUE)

#Check data is read in correctly
View(LDA)
head(LDA)

#Check Dimensions of Dataset
dim(LDA)
#4653 Observations 9 variables

#Check Variable Names
names(LDA)

#Check Data structure
str(LDA)

LDA$Education <- as.factor(LDA$Education)
class(LDA$Education)

LDA$City <- as.factor(LDA$City)
class(LDA$City)

LDA$Gender <- as.factor(LDA$Gender)
class(LDA$Gender)

LDA$EverBenched <- as.factor(LDA$EverBenched)
class(LDA$EverBenched)

LDA$LeaveOrNot <- as.factor(LDA$LeaveOrNot)
class(LDA$LeaveOrNot)

str(LDA)
head(LDA)
library(MASS)

#With Cross Validation
# The dependent variable must be categorical
LDA.OP <- lda(LeaveOrNot ~ ., data=LDA, CV=TRUE)
LDA.OP

#To Plot the Data, you cannot use CV
LDA.OP <- lda(LeaveOrNot ~ ., data=LDA)
LDA.OP

plot(LDA.OP, xlab = "LD1", ylab = "LD2")

# Try to predict the class from the original data
# Note ... this is JUST a test to see how this works
# In practice you will want to use cross-validation!

##Predicting training results.
p <- predict(LDA.OP, newdata=LDA[,1:8])$class
p


# Compare the results of the prediction (Confusion Matrix)
table1<-table(p, LDA$LeaveOrNot)
table1

#Using Trace
sum(diag(table1)/sum(table1))

accuracy <- (2743+692)/(2743+692+310+908)
accuracy
#0.738


#Creating Training and Testing Samples
require(caTools)  # loading caTools library
library(caTools)
set.seed(123)   # set seed to ensure you always have same random numbers generated
sample = sample.split(LDA,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(LDA,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(LDA, sample==FALSE)

# The dependent variable must be categorical (Assuming No Cross-Validation)
class(LDA$LeaveOrNot)
library(MASS)
employeeLDA = lda(LeaveOrNot ~ ., data=train)
employeeLDA

plot(employeeLDA)


# Try to predict the class from the original data
# Note ... this is JUST a test to see how this works
# In practice you will want to use cross-validation!
p <- predict(employeeLDA, newdata=train[,1:8])$class
p

# Compare the results of the prediction (Confusion Matrix)
table2<-table(p, train$LeaveOrNot)
table2

accuracy <- (1819+448)/(1819+448+212+623)
accuracy
#0.73

mean(p== train$LeaveOrNot)
#0.73

# Setting "CV = T" will have the lda function perform
# "Leave-one-out" cross-validation
employeeLDA = lda(LeaveOrNot ~ ., data=train, CV=T)
employeeLDA

table(employeeLDA$class, train$LeaveOrNot)

coef(employeeLDA)

library(ggplot2)
library(caret)
modelFit<- train(LeaveOrNot ~ ., method='lda',preProcess=c('scale', 'center'), data=train)

#Confusion Matrix
confusionMatrix(train$LeaveOrNot, predict(modelFit, train))

########################################################

#install packages
install.packages("FSelector")
install.packages("rpart")
install.packages("caret", dependencies = TRUE)
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("xlsx")
install.packages("data.tree")
install.packages("caTools")

library(FSelector)
library(rpart)
library(caret)
library(dplyr)
library(rpart.plot)
library(xlsx)
library(data.tree)
library(caTools)

#Set Working Directory
setwd("~/Downloads/02_IS 507/Final Project/Datasets")

#Read in Dataset
library(readr)
Airbnb <- read.csv(file="Airbnb.csv", header=TRUE, sep=",")


##Show the column name
names(Airbnb)
View(Airbnb)
dim(Airbnb)
#6,366 observations and 7 variables

#Check Missing Data
sum(is.na(Airbnb))
Airbnb2 <- na.omit(Airbnb)
sum(is.na(Airbnb2))

#Check Data structure
str(Airbnb2)

Airbnb2$host_response_rate <- as.numeric(Airbnb2$host_response_rate)
Airbnb2$host_acceptance_rate <- as.numeric(Airbnb2$host_acceptance_rate)
Airbnb2$host_neighbourhood <- as.factor(Airbnb2$host_neighbourhood)
Airbnb2$host_has_profile_pic <- as.factor(Airbnb2$host_has_profile_pic)
Airbnb2$host_identity_verified <- as.factor(Airbnb2$host_identity_verified)
Airbnb2$host_is_superhost <- as.factor(Airbnb2$host_is_superhost)

str(Airbnb2)


#Suffle the dataset
set.seed(42)
rows <- sample(nrow(Airbnb2))
Airbnb3 <- Airbnb2[rows, ]
View(Airbnb3)


#Split the data into training and test sets with an 80-20 split
library(dplyr)
train <- sample_frac(Airbnb3, 0.8)  
sid <-as.numeric(rownames(train)) 
test <- Airbnb3[-sid,] 


#Train a decision tree classifier on the training set
#and predict the class labels in the test set
library(rpart)
library(rpart.plot)
#train the decision tree classifier
fit <- rpart(host_is_superhost ~., data = train, method = 'class')

#prediction
predict <-predict(fit, test, type = 'class')
table_mat <- table(test$host_is_superhost, predict)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
