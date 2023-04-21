##IS507_Assignment 4:Linear Regression and Overfitting
##ChengHsuan Lin / 10172021

#Setup Working Directory
setwd("~/Downloads/02_IS 507/R files")

#Read in Data
library(readr)
MUREG.raw <- read_csv("Student_Grade_Prediction_Assignment 4.csv", col_names = TRUE)
head(MUREG.raw) 
View(MUREG.raw) 

#Check Dimensions of Dataset
dim(MUREG.raw)
#395 Observations 33 variables

#Check Variable Names
MUREG<-MUREG.raw[,c(1:30,33)]
MUREG
summary(MUREG)
names(MUREG)

#Check Data structure
str(MUREG) 

#Need to change a few variables to read as numeric variables instead of categorical variables
library(plyr)

table(MUREG$school)
MUREG$school_num <- revalue(MUREG$school, c("GP"="1", "MS"="2"))
MUREG$school_num <- as.numeric(MUREG$school)

table(MUREG$sex)
MUREG$sex_num <- revalue(MUREG$sex, c("F"="0", "M"="1"))
MUREG$sex_num <- as.numeric(MUREG$sex)

table(MUREG$address)
MUREG$address_num <- revalue(MUREG$address, c("R"="1", "U"="2"))
MUREG$address_num <- as.numeric(MUREG$address)

table(MUREG$famsize)
MUREG$famsize_num <- revalue(MUREG$famsize, c("GT3"="1", "LE3"="2"))
MUREG$famsize_num <- as.numeric(MUREG$famsize)

table(MUREG$Pstatus)
MUREG$Pstatus_num <- revalue(MUREG$Pstatus, c("A"="1", "T"="2"))
MUREG$Pstatus_num <- as.numeric(MUREG$Pstatus)

table(MUREG$schoolsup)
MUREG$schoolsup_num <- revalue(MUREG$schoolsup, c("no"="0", "yes"="1"))
MUREG$schoolsup_num <- as.numeric(MUREG$schoolsup)

table(MUREG$famsup)
MUREG$famsup_num <- revalue(MUREG$famsup, c("no"="0", "yes"="1"))
MUREG$famsup_num <- as.numeric(MUREG$famsup)

table(MUREG$paid)
MUREG$paid_num <- revalue(MUREG$paid, c("no"="0", "yes"="1"))
MUREG$paid_num <- as.numeric(MUREG$paid)

table(MUREG$activities)
MUREG$activities_num <- revalue(MUREG$activities, c("no"="0", "yes"="1"))
MUREG$activities_num <- as.numeric(MUREG$activities)

table(MUREG$nursery)
MUREG$nursery_num <- revalue(MUREG$nursery, c("no"="0", "yes"="1"))
MUREG$nursery_num <- as.numeric(MUREG$nursery)

table(MUREG$higher)
MUREG$higher_num <- revalue(MUREG$higher, c("no"="0", "yes"="1"))
MUREG$higher_num <- as.numeric(MUREG$higher)

table(MUREG$internet)
MUREG$internet_num <- revalue(MUREG$internet, c("no"="0", "yes"="1"))
MUREG$internet_num <- as.numeric(MUREG$internet)

table(MUREG$romantic)
MUREG$romantic_num <- revalue(MUREG$romantic, c("no"="0", "yes"="1"))
MUREG$romantic_num <- as.numeric(MUREG$romantic)

str(MUREG) 

#Check Missing Data
sum(is.na(MUREG))
#No Missing Data

###############################################################################
#Using Manual Multiple Linear Regression
#Create Initial Linear Regression Model with Enter Method
MUREG1<-MUREG[,c(3,7,8,13:15,24:30,31:48)]

model1 <- lm(G3 ~ ., data=MUREG1)
model1

#Check VIF
library(DescTools)
VIF(model1)



#Check Spearman Correlations
library(corrplot)
corrplot(cor(MUREG1, method = "spearman"))
corrplot(cor(MUREG1, method = "spearman"), method="number")
names(MUREG1)




#run multiple regression
MUREG.OP <-lm ( G3 ~ ., data= MUREG1)
summary(MUREG.OP)

#run standardized multiple regression
library(lm.beta)
Std.MUREG.OP <-lm.beta(MUREG.OP)
summary(Std.MUREG.OP)

#Removed variables that are not significantly effect G3
names(MUREG1)
MUREG.OP.2 <- MUREG1[,c(6,9,14,16)]

names(MUREG.OP.2)

model2 <- lm(G3 ~ ., data=MUREG.OP.2)
model2


library(DescTools)
VIF(model2)




#What are issues with the model?

summary(model2)

#Diagnostic Plots for Model Fit
par(mfrow = c(2, 2))
plot(model2)

library(ggfortify)
autoplot(model2)

par(mfrow = c(1, 1))
plot(model1)
library(ggfortify)
autoplot(model1)

################################################################################

#Using Stepwise Multiple Linear Regression

null = lm(G3 ~ 1-G3, data=MUREG.OP.2)
null

full = lm(G3 ~ .-G3, data=MUREG.OP.2)
full

#Forward Regression
train_Forward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(train_Forward)

#Backward Regression
train_Backward = step(full, direction="backward")
summary(train_Backward)

#Stepwise Regression
train_Step = step(null, scope = list(upper=full), direction="both")
summary(train_Step)

################################################################################
#Regularized Regression

x=model.matrix(G3 ~ ., data=MUREG.OP.2)
y=MUREG.OP.2$G3

library(glmnet)

#Ridge Regression
cv.ridge <- cv.glmnet(x,y, typemeasure="mse", alpha=0)

#Different functions available in cv.ridge
ls(cv.ridge)

plot(cv.ridge)

coef(cv.ridge)



#plot variable feature coefficients against shrinkage parameter lambda

glmmod <- glmnet(x, y, alpha = 0)
plot(glmmod, xvar="lambda")
grid()

#Lasso Regression

cv.lasso <- cv.glmnet(x,y, typemeasure="mse", alpha=1)
cv.lasso

ls(cv.lasso)

plot(cv.lasso)

Lambda.best <- cv.lasso$lambda.min

predict(cv.lasso, s = Lambda.best, type = "coefficients")

fit<-cv.lasso$glmnet.fit
fit



#ggplot2
library(ggplot2)

ggplot(MUREG.OP.2, aes(x = failures+sex_num+goout, y = G3)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP.2, aes(x = failures, y = G3)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP.2, aes(x = sex_num, y = G3)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP.2, aes(x = goout, y = G3)) +
  geom_point(size=3) +stat_smooth(method="lm")
