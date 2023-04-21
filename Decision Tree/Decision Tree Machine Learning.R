#R Lab 3 - Stress Echo Machine Learning Techniques

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


#Check Frequencies of Gender, hxofCig, and ecg Variables

#Gender
table(stress_echo$gender)

#hxofCig
table(stress_echo$hxofCig)

#ecg
table(stress_echo$ecg)


#Check Normality of Age and Plot Age

shapiro.test(stress_echo$age)
#P-value<0.05 Means Age is not normal


#Histogram
hist(stress_echo$age, col="blue", freq=TRUE, xlab = "Patient Age", ylab = "Frequency", main = "Patient Age")


#Create an Age Categorical Variable using Tertiles
#Uses the cutoff points 63 and 73 in SAS
#Use the cutoff points 48.3 and 70.7 in R

stress_echo$age_tertile <-cut(stress_echo$age,3)
#(25.9,48.3] (48.3,70.7] (70.7,93.1]
#Cuts data into intervals of the same length

table(stress_echo$age_tertile)


stress_echo$age_tertile1 <-cut(stress_echo$age, c(0, 63, 73,100))

table(stress_echo$age_tertile1)

stress_echo$age_tertile2<-NULL
stress_echo$age_tertile2[(stress_echo$age >= 0) & (stress_echo$age <= 63)] <- "Age LT 63"
stress_echo$age_tertile2[(stress_echo$age > 63) & (stress_echo$age<= 73)] <- "Age Between 63 and 73"
stress_echo$age_tertile2[(stress_echo$age > 73) & (stress_echo$age<=100)] <- "Age GT 73"

table(stress_echo$age_tertile2)


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

#Libraries

library(dplyr)     # data wrangling
library(ggplot2)   # plotting
library(rsample)   # training and testing splitting
library(caret)     # for logistic regression modeling and prediction outputs
library(vip)       # variable importance


# Create training (70%) and test (30%) sets

set.seed(123)  # use a set seed point for reproducibility
split <- initial_split(stress_echo, prop = .7, strata = "any.event")
train <- training(split)
test  <- testing(split)


#Logistic Regression

#For explaining dependent variable

stress_echo$any.event <- as.factor(stress_echo$any.event)

log_reg <- glm(
  any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num,
  family = "binomial", 
  data = stress_echo
)

summary(log_reg) #Coefficients Not in exponential form

tidy(log_reg) #Coefficients Not in exponential form

#Coefficients in exponential form
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE) 

train$any.event<- as.factor(train$any.event)

#For Predicting dependent variable
log_reg = train(
  form = any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num,
  data = train,
  method = "glm",
  family = "binomial"
)

#Confusion Matrix
confusionMatrix(predict(log_reg, test), as.factor(test$any.event))


#Variables of Importance

vip(log_reg, num_features = 10)


#ROC Curves

log_reg_train <- glm(any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num, data=train, family=binomial)


#ROC Curves
library(ROCR)

log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
log_reg_test_prob

preds <- prediction(as.numeric(log_reg_test_prob), test$any.event)

perf <- performance(preds,"tpr","fpr")
plot(perf,colorize=TRUE)


library(precrec)
precrec_obj <- evalmod(scores = log_reg_test_prob, labels = test$any.event)
autoplot(precrec_obj)

## Get AUCs
sm_aucs <- auc(precrec_obj)
## Shows AUCs
sm_aucs

precrec_obj2 <- evalmod(scores = log_reg_test_prob, labels = test$any.event, mode="basic")
autoplot(precrec_obj2)   


library(ROCit)
## Warning: package 'ROCit' was built under R version 3.5.2
ROCit_obj <- rocit(score=log_reg_test_prob,class=test$any.event)
plot(ROCit_obj)

summary(ROCit_obj)

measure <- measureit(score = log_reg_test_prob, class = test$any.event,
                     measure = c("ACC", "MIS", "SENS", "SPEC", "PREC", "REC","PPV","NPV", "FSCR"))
measure



#Decision Tree

decision_tree = train(any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num, 
                      data=train, 
                      method="rpart", 
)

decision_tree

summary(decision_tree$finalModel) # detailed summary of splits

#predict

confusionMatrix(predict(decision_tree, test), as.factor(test$any.event))


# plot tree
library(rpart.plot)

plot.model <- rpart(any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num, 
                    data=train, cp = .02)
rpart.plot(plot.model)


#Random Forest

library(randomForest)

randomForest <- randomForest(any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num,   data=train)
print(randomForest) # view results

importance(randomForest) # importance of each predictor

vip(randomForest, num_features = 10)

#predict
rfPredict <- predict(randomForest,test)
confusionMatrix(rfPredict, as.factor(test$any.event))


#KNN

knn <- train(any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num, data = train, method = "knn", preProcess = c("center","scale"), tuneLength = 3)
knn

plot(knn)

knnPredict <- predict(knn,test)

confusionMatrix(knnPredict, as.factor(test$any.event))


#Naive Bayes

library(naivebayes)

x<-train[,c(3, 16, 25, 26, 28, 30)]
y<-train$any.event

naive_bayes = train(x,y,'nb')
any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num


naive_bayes


#prediction

predict <- predict(naive_bayes,test)
confusionMatrix(predict, as.factor(test$any.event))


#Support Vector Machine

#SVM Linear

# Fit the model 
svm1 <- train(any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num, data = train, method = "svmLinear", preProcess = c("center","scale"))

#View the model
svm1

#prediction

svm1_predict <- predict(svm1,test)
confusionMatrix(svm1_predict, as.factor(test$any.event))


#Non-Linear

svm2 <- train(any.event ~ basebp + baseEF + hxofHT + hxofDM + hxofMI + hxofCABG + gender_num + ecg_num, data = train, method = "svmRadial", preProcess = c("center","scale"), tuneLength = 10)

#View the model
svm2

#prediction

svm2_predict <- predict(svm2,test)
confusionMatrix(svm2_predict, as.factor(test$any.event))


