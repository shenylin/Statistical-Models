#Linear Regression and Overfitting

#Setup Working Directory

setwd("C:/Users/sbess/Downloads")

#Read in Data

library(readr)

stress_echo <- read_csv("stressEcho.csv", col_names = TRUE)


#Check data is read in correctly
View(stress_echo)

head(stress_echo)


#Check Dimensions of Dataset
dim(stress_echo)
#558 Observations 32 variables


#Check Variable Names
names(stress_echo)


#Check Data structure
str(stress_echo)


#Check Missing Data
sum(is.na(stress_echo))
#No Missing Data


'Variables Defined

Explanation of Data Measurement Abbreviations in the Data File

bhr	basal heart rate
basebp	basal blood pressure
basedp	basal double product (= bhr x basebp)
pkhr	peak heart rate
sbp	systolic blood pressure
dp	double product (= pkhr x sbp)
dose	dose of dobutamine given
maxhr	maximum heart rate
%mphr(b)	% of maximum predicted heart rate achieved
mbp	maximum blood pressure
dpmaxdo	double product on maximum dobutamine dose
dobdose	dobutamine dose at which maximum double product occured
age	age
gender	gender
baseef	baseline cardiac ejection fraction (a measure of the heart pumping efficiency)
dobef	ejection fraction on dobutamine
chestpain	1 means experienced chest pain
posecg	signs of heart attack on ecg (1 = yes)
equivecg	ecg is equivocal (1 = yes)
restwma	cardiologist sees wall motion anamoly on echocardiogram (1 = yes)
posse	stress echocardiogram was positive (1 = yes)
newmi	new myocardial infarction, or heart attack (1 = yes)
newptca	recent angioplasty (1 = yes)
newcabg	recent bypass surgery (1 = yes)
death	died (1 = yes)
hxofht	history of hypertension (1 = yes)
hxofdm	history of diabetes (1 = yes)
hxofcig	history of smoking (1 = yes)
hxofmi	history of heart attack (1 = yes)
hxofptca	history of angioplasty (1 = yes)
hxofcabg	history of bypass surgery (1 = yes)
any event	Outcome variable, defined as "death or newmi or newptca or newcabg". if any of these variables is positive (= 1) then "any event" is also postive (= 1).'

stress_echo$age_tertile1 <-cut(stress_echo$age, c(0, 63, 73,100))

table(stress_echo$age_tertile1)

stress_echo$age_tertile2<-NULL
stress_echo$age_tertile2[(stress_echo$age >= 0) & (stress_echo$age <= 63)] <- "Age LT 63"
stress_echo$age_tertile2[(stress_echo$age > 63) & (stress_echo$age<= 73)] <- "Age Between 63 and 73"
stress_echo$age_tertile2[(stress_echo$age > 73) & (stress_echo$age<=100)] <- "Age GT 73"

table(stress_echo$age_tertile2)


stress_echo$age_tertile3<-NULL
stress_echo$age_tertile3[(stress_echo$age >= 0) & (stress_echo$age <= 63)] <- 0
stress_echo$age_tertile3[(stress_echo$age > 63) & (stress_echo$age<= 73)] <- 1
stress_echo$age_tertile3[(stress_echo$age > 73) & (stress_echo$age<=100)] <- 2

table(stress_echo$age_tertile3)



#Need to change Gender, hxofCig, and ecg variables to read as numeric variables instead of categorical variables

library(plyr)

#Gender

table(stress_echo$gender)

stress_echo$gender_num <- revalue(stress_echo$gender, c("male"="1", "female"="0"))

stress_echo$gender_num <- as.numeric(stress_echo$gender_num)

#hxofCig

table(stress_echo$hxofCig)

stress_echo$hxofCig_num <- revalue(stress_echo$hxofCig, c("non-smoker"="0", "moderate"="1", "heavy"="2"))

stress_echo$hxofCig_num <- as.numeric(stress_echo$hxofCig_num)

#ecg

table(stress_echo$ecg)

stress_echo$ecg_num <- revalue(stress_echo$ecg, c("normal"="0", "equivocal"="1", "MI"="2"))

stress_echo$ecg_num <- as.numeric(stress_echo$ecg_num)

str(stress_echo)


#Research Question: What explains a patient's baseline systolic blood pressure?

names(stress_echo)

reg_dataset <- stress_echo[, c(2:4,8, 16, 18:20, 25, 26, 28:31, 36:38)]

str(reg_dataset)

#Check Spearman Correlations

library(corrplot)

corrplot(cor(reg_dataset, method = "spearman"))

corrplot(cor(reg_dataset, method = "spearman"), method="number")

#Removed basedp

names(reg_dataset)

reg_dataset2 <- reg_dataset[,c(1:2,4:17)]

names(reg_dataset2)
###############################################################################

#Using Manual Multiple Linear Regression

#Create Initial Linear Regression Model with Enter Method

model1 <- lm(basebp ~ ., data=reg_dataset)
model1

model2 <- lm(basebp ~ ., data=reg_dataset2)
model2

#Check VIF
library(DescTools)
VIF(model2)

#What are issues with the model?

summary(model2)

#Diagnostic Plots for Model Fit
par(mfrow = c(2, 2))
plot(model1)

library(ggfortify)
autoplot(model1)

par(mfrow = c(1, 1))
################################################################################

#Using Stepwise Multiple Linear Regression

null = lm(basebp ~ 1-basedp, data=reg_dataset2)
null

full = lm(basebp ~ .-basedp, data=reg_dataset2)
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

x=model.matrix(basebp ~ ., data=reg_dataset2)
y=reg_dataset2$basebp

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


#Ordinary Least Squares Regression

model2 <- lm(basebp ~ dose+hxofHT+hxofDM+hxofPTCA+gender_num, data=reg_dataset)
summary(model2)