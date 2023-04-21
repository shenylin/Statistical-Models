##IS507 Assignment 8
##ChengHsuan Lin / 12052021
##Dataset: Employee-1.csv

#Setup Working Directory
setwd("~/Downloads/02_IS 507/R files")

#Read in Data
library(readr)
Employee <- read_csv("Employee-1.csv", col_names = TRUE)

#Check data is read in correctly
View(Employee)
head(Employee)

#Check Dimensions of Dataset
dim(Employee)
#4653 Observations 9 variables

#Check Variable Names
names(Employee)

library(MASS)
#Check Missing Data
sum(is.na(Employee))
#No Missing Data

#Check Normality of Age and Plot Age
shapiro.test(Employee$Age)
#P-value < .05 means Age is not normal

#Histogram
hist(Employee$Age, col="blue", freq=TRUE, xlab = "Employee Age", ylab = "Frequency", main = "Employee Age")

#Check Data structure
str(Employee)
Employee$City <- as.factor(Employee$City)

#Need to change Education, Gender and EverBenched variables to read as numeric variables instead of categorical variables
str(Employee)
library(plyr)

#Education
table(Employee$Education)
Employee$Education_num <- revalue(Employee$Education, c("Bachelors"="1", "Masters"="2", "PHD"="3"))
Employee$Education_num <- as.numeric(Employee$Education_num)

#Gender
table(Employee$Gender)
Employee$Gender_num <- revalue(Employee$Gender, c("Male"="1", "Female"="0"))
Employee$Gender_num <- as.numeric(Employee$Gender_num)

#EverBenched
table(Employee$EverBenched)
Employee$EverBenched_num <- revalue(Employee$EverBenched, c("No"="0", "Yes"="1"))
Employee$EverBenched_num <- as.numeric(Employee$EverBenched_num)

str(Employee)

#Libraries

library(dplyr)     # data wrangling
library(ggplot2)   # plotting
library(rsample)   # training and testing splitting
library(caret)     # for logistic regression modeling and prediction outputs
library(vip)       # variable importance


# Create training (70%) and test (30%) sets

set.seed(123)  # use a set seed point for reproducibility
split <- initial_split(Employee, prop = .7, strata = "LeaveOrNot")
train <- training(split)
test  <- testing(split)


#Logistic Regression
#For explaining dependent variable
Employee$LeaveOrNot <- as.factor(Employee$LeaveOrNot)

log_reg <- glm(
  LeaveOrNot ~ Education_num + JoiningYear + City + PaymentTier + Age + Gender_num + EverBenched_num + ExperienceInCurrentDomain,
  family = "binomial", 
  data = Employee
)

summary(log_reg) #Coefficients Not in exponential form

tidy(log_reg) #Coefficients Not in exponential form

#Coefficients in exponential form
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE) 

train$LeaveOrNot<- as.factor(train$LeaveOrNot)

#For Predicting dependent variable
log_reg = train(
  form = LeaveOrNot ~ Education_num + JoiningYear + City + PaymentTier + Age + Gender_num + EverBenched_num + ExperienceInCurrentDomain,
  data = train,
  method = "glm",
  family = "binomial"
)

pred <- predict(log_reg, test)
pred

#Confusion Matrix
confusionMatrix(pred, as.factor(test$LeaveOrNot))

#Variables of Importance
vip(log_reg, num_features = 10)

#ROC Curves
log_reg_train <- glm(LeaveOrNot ~ Education_num + JoiningYear + City + PaymentTier + Age + Gender_num + EverBenched_num + ExperienceInCurrentDomain + Education_num + JoiningYear + City + PaymentTier + Age + Gender_num + EverBenched_num + ExperienceInCurrentDomain, data=train, family=binomial)

library(ROCR)
log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
log_reg_test_prob

preds <- prediction(as.numeric(log_reg_test_prob), test$LeaveOrNot)

perf <- performance(preds,"tpr","fpr")
plot(perf,colorize=TRUE)


library(precrec)
precrec_obj <- evalmod(scores = log_reg_test_prob, labels = test$LeaveOrNot)
autoplot(precrec_obj)

## Get AUCs
sm_aucs <- auc(precrec_obj)
## Shows AUCs
sm_aucs

precrec_obj2 <- evalmod(scores = log_reg_test_prob, labels = test$LeaveOrNot, mode="basic")
autoplot(precrec_obj2)   


library(ROCit)

## Warning: package 'ROCit' was built under R version 3.5.2
ROCit_obj <- rocit(score=log_reg_test_prob,class=test$LeaveOrNot)
plot(ROCit_obj)

summary(ROCit_obj)

measure <- measureit(score = log_reg_test_prob, class = test$LeaveOrNot,
                     measure = c("ACC", "MIS", "SENS", "SPEC", "PREC", "REC","PPV","NPV", "FSCR"))
measure
