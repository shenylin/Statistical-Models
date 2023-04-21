## IS 507 Final Project
## ChengHsuan Lin
## Dataset: Chicago Airbnb.csv

#Setup Working Directory
setwd("~/Downloads/02_IS 507/Final Project/Datasets")

#Read in Data
library(readr)
Airbnb <- read_csv("Chicago Airbnb.csv", col_names = TRUE)


#Check data is read in correctly
View(Airbnb)
names(Airbnb)
head(Airbnb)

#Check Dimensions of Dataset
dim(Airbnb)
#6528 observations 14 variables

#Check Data structure
str(Airbnb)

#Check Missing Data
sum(is.na(Airbnb))

#Ordinary Least Squares Regression
Airbnb.OLS <- lm(review_scores_rating ~ ., data=Airbnb)
summary(Airbnb.OLS)

#Check VIF
library(DescTools)
VIF(Airbnb.OLS)

#Diagnostic Plots for Model Fit
par(mfrow = c(2, 2))
plot(Airbnb.OLS)

library(ggfortify)
autoplot(Airbnb.OLS)
par(mfrow = c(1, 1))

glmmod <- glmnet(x, y, alpha = 0)
plot(glmmod, xvar="lambda")
grid()

#ggplot2
library(ggplot2)

ggplot(Airbnb.OLS, aes(x = ., y = review_scores_rating) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(Airbnb.OLS, aes(x = host_response_rate+bedrooms+host_verifications_selfie+
                         host_verifications_jumio+host_verifications_review
                       +host_verifications_offline_government_id+host_verifications_email
                       +host_verifications_government_id, y = review_scores_rating)) +
    geom_point(size=3) +stat_smooth(method="lm")

library(dplyr)
library(ggplot2)
Airbnb.OLS %>%
  ggplot(aes(x = host_response_rate+bedrooms+host_verifications_selfie+
               host_verifications_jumio+host_verifications_offline_government_id+host_verifications_email
             +host_verifications_government_id, y = review_scores_rating)) +
  geom_point(colour = "red")

#Communication
Airbnb.OLS %>%
  ggplot(aes(x = host_response_rate, y = review_scores_rating)) +
  geom_point(colour = "red")

#Space Offered
Airbnb.OLS %>%
  ggplot(aes(x = bedrooms, y = review_scores_rating)) +
  geom_point(colour = "red")

#Host Verification Information
Airbnb.OLS %>%
  ggplot(aes(x = host_verifications_selfie+
                 host_verifications_jumio+
                 host_verifications_offline_government_id+
                 host_verifications_email+ 
                 host_verifications_government_id, y = review_scores_rating)) +
  geom_point(colour = "red")

Airbnb.OLS %>%
  ggplot(aes(x = host_verifications_offline_government_id, y = review_scores_rating)) +
  geom_point(colour = "red")

##########################################################################
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

