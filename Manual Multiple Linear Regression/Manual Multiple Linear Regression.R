##IS507_Assignment 6
##ChengHsuan Lin / 11072021

#Setup Working Directory
setwd("~/Downloads/02_IS 507/Final Project/Datasets")

#Read in Data
library(readr)
MUREG.raw <- read_csv("listings.csv", col_names = TRUE)

#Check data is read in correctly
head(MUREG.raw) 
View(MUREG.raw) 

#Check Dimensions of Dataset
dim(MUREG.raw)
#6366 Observations of 74 variables

#Check Variable Names
names(MUREG.raw)

#Check Data structure
str(MUREG.raw)
MUREG <- MUREG.raw[,c(40,62:67)]
MUREG
summary(MUREG)
names(MUREG)

#Check Data structure
str(MUREG) 

#Check Missing Data
sum(is.na(MUREG))
#No Missing Data

#Listwise Deletion
MUREG_2 <- na.omit(MUREG)
sum(is.na(MUREG_2))

###############################################################################
#Using Manual Multiple Linear Regression
#Create Initial Linear Regression Model with Enter Method

model1 <- lm(price ~ ., data=MUREG_2)
model1

#Check VIF
library(DescTools)
VIF(model1)

#Check Spearman Correlations
library(corrplot)
corrplot(cor(MUREG_2, method = "spearman"))
corrplot(cor(MUREG_2, method = "spearman"), method="number")

#run multiple regression
MUREG.OP <-lm ( price ~ ., data= MUREG_2)
summary(MUREG.OP)

#run standardized multiple regression
library(lm.beta)
Std.MUREG.OP <-lm.beta(MUREG.OP)
summary(Std.MUREG.OP)

#Removed variables that are not significantly effect price
MUREG.OP.2 <- MUREG_2[,c(1,6,7)]

names(MUREG.OP.2)

model2 <- lm(price ~ ., data=MUREG.OP.2)
model2


library(DescTools)
VIF(model2)

summary(model2)

#ggplot2
library(ggplot2)

ggplot(MUREG.OP.2, aes(x = review_scores_location+review_scores_value, y = price)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP.2, aes(x = review_scores_location, y = price)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP.2, aes(x = review_scores_value, y = price)) +
  geom_point(size=3) +stat_smooth(method="lm")

##############################################################################################
library(foreign)
library(CCA)
library(yacca)
library(MASS)

#Read in Young People Survey Data
setwd("~/Downloads/02_IS 507/W11 Canonical Correlation Analysis/Assignment 6")

#Read in Datasets
mmreg = read.csv("responses.csv", header = TRUE, sep = ",")
head(mmreg)

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(mmreg))
#571 total missing values (571 cells with missing data)

#Treat Missing Values
#Listwise Deletion
mmreg2 <- na.omit(mmreg)

#Check new data has no missing data
sum(is.na(mmreg2))

###################################################################
# Exploring correlations between music and spending

#Create Sets of  Variables
music = mmreg2[, 1:19]
spending = mmreg2[,134:140]

library(psych)
names(music)
str(music)
describe(music)

names(spending)
str(spending)
describe(spending)


#Base Package
# This gives us the canonical correlates, but no significance tests
c = cancor(music, spending)
c
####################################################################

#CCA library
library(CCA)

#Breakdown of the Correlations
matcor(music, spending)

#Correlations between sepal and sepal (X)
#Correlations between petal and petal (Y)
cc_mm = cc(music, spending)
cc_mm$cor

#Funcrions for CCA
ls(cc_mm)

#XCoef Correlations
cc_mm$xcoef

#YCoef Correlations
cc_mm$ycoef

#Calculate Scores
loadings_mm = comput(music, spending, cc_mm)
ls(loadings_mm)

#Correlation X Scores
loadings_mm$corr.X.xscores

#Correlation Y Scores
loadings_mm$corr.Y.yscores

# A basic visualization of the canonical correlation
plt.cc(cc_mm)

################################################################
# With yacca
################################################################

library(yacca)
c2 = cca(music, spending)
c2

#CV1
helio.plot(c2, cv=1, x.name="Music Values", 
           y.name="Spending Values")

#CV2
helio.plot(c2, cv=2, x.name="Music Values", 
           y.name="Spending Values")

#Function Names
ls(c2)

# Perform a chi-square test on C2
c2
ls(c2)
c2$chisq
c2$df
summary(c2)
round(pchisq(c2$chisq, c2$df, lower.tail=F), 3)