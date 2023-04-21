
library(MASS)

data(iris)
head(iris)
str(iris)

#Graph Data
library(psych)
pairs.panels(iris[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[iris$Species],
             pch = 21)

#With Cross Validation
# The dependent variable must be categorical
irisLDA <- lda(Species ~ ., data=iris, CV=TRUE)
irisLDA

#To Plot the Data, you cannot use CV
irisLDA <- lda(Species ~ ., data=iris)
irisLDA

plot(irisLDA, xlab = "LD1", ylab = "LD2")

# Try to predict the class from the original data
# Note ... this is JUST a test to see how this works
# In practice you will want to use cross-validation!
p <- predict(irisLDA, newdata=iris[,1:4])$class
p

# Compare the results of the prediction (Confusion Matrix)
table1<-table(p, iris$Species)
table1

#Using Trace
sum(diag(table1)/sum(table1))

accuracy <- (50+48+49)/(50+48+49+1+2)
accuracy
#0.98

mean(p== iris$Species)
#0.98

#Creating Training and Testing Samples
require(caTools)  # loading caTools library
library(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(iris,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(iris,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(iris, sample==FALSE)

# The dependent variable must be categorical (Assuming No Cross-Validation)
irisLDA = lda(Species ~ ., data=train)
irisLDA

plot(irisLDA)


# Try to predict the class from the original data
# Note ... this is JUST a test to see how this works
# In practice you will want to use cross-validation!

p <- predict(irisLDA, train)
#Stacked Histogram of LDA Functions
ldahist(data=prd$x[,1], g = train$Species)

# Compare the results of the prediction
table(p, train$Species)

accuracy <- (30+29+30)/(30+29+30+1)
accuracy
#0.99

mean(p== train$Species)
#0.99

#LDA Visualization
#install.packages("devtools")
library(devtools)
install_github("fawda123/ggord")
library(ggord)
ggord(irisLDA, train$Species, ylim = c(-5, 5 ))

#Other Visualizations

newdata <- data.frame(train$Species, lda = prd$x)

library(ggplot2)
ggplot(newdata) + geom_point(aes(lda.LD1, lda.LD2, colour = train$Species), size = 2.5)


# Setting "CV = T" will have the lda function perform
# "Leave-one-out" cross-validation
irisLDA2 = lda(Species ~ ., data=train, CV=T)
irisLDA2

table(irisLDA2$class, train$Species)

coef(irisLDA)


library(caret)
modelFit<- train(Species ~ ., method='lda',preProcess=c('scale', 'center'), data=train)

#Confusion Matrix
confusionMatrix(train$Species, predict(modelFit, train))

#Visualization
library(klaR)
partimat(Species ~ ., data = train, method = "lda")