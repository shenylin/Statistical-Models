##Assignment 3
##ChengHsuan Lin / 10092021

#Setup Working Directory
setwd("~/Downloads/02_IS 507/R files")

#Read in Data
library(readr)
INDAOV.raw <- read_csv("Student_Grade_Prediction.csv", col_names = TRUE)
head(INDAOV.raw) 
View(INDAOV.raw) 

#Check Variable Names
INDAOV<-INDAOV.raw[c(14,33)]
INDAOV
summary(INDAOV)
names(INDAOV)

#Check Data structure
str(INDAOV) 
class(INDAOV$studytime)

#Check Data structure
INDAOV$studytime <-as.factor(INDAOV$studytime)
class(INDAOV$studytime)

#Check Missing Data
sum(is.na(INDAOV))
#No Missing Data

#check skewness and kurtosis
library(psych)
describe(INDAOV[1:2])
#skewness and kurtosis are between -3 and 3 => data normality checked

#Bartlett test of homogeneity of variances
bartlett.test( G3 ~ studytime, data=INDAOV)

#Levene's Test for Homogeneity of Variance 
library(car)
leveneTest(G3 ~ studytime, data=INDAOV)

#1. Does study time affect the students??? grades on the third period exam?
INDAOV.op  <- aov(G3 ~ studytime, data=INDAOV)
summary(INDAOV.op)

#boxplot
library(car)
library(sandwich)
library(RcmdrMisc)
with(INDAOV, boxplot(G3 ~ studytime, error.bars="se"))
title(xlab="studytime", ylab="final grade")

#2. What is the relationship or association between first period exam and absences?
library(readr)
COR.raw <- read_csv("Student_Grade_Prediction.csv", col_names = TRUE)
COR <- COR.raw[c(30,31)]
colnames(COR) <- c("absences","first period exam")
str(COR) 


library(car)
library(sandwich)
library(RcmdrMisc)
PEARSON.r <- rcorr.adjust(COR, type="pearson")
PEARSON.r


library(corrplot)
corrplot(PEARSON.r$R$r ,method = "circle")
corrplot(PEARSON.r$R$r ,method = "ellipse")

corrplot.mixed(PEARSON.r$R$r ,lower = "number",upper = "ellipse")


library(apaTables)
apa.cor.table(COR, filename="Student_Grade_Prediction.doc")



#3. Show and explain a visualization of correlations 
#between the second exam and variables: family relationships to absences

#Setup Working Directory
setwd("~/Downloads/02_IS 507/R files")

#Correlation
library(readr)
COR.raw2 <- read_csv("Student_Grade_Prediction.csv", col_names = TRUE)
COR2 <- COR.raw2[c(24:30,32)]
str(COR2) 


library(car)
library(sandwich)
library(RcmdrMisc)
PEARSON.r <- rcorr.adjust(COR2, type="pearson")
PEARSON.r


library(corrplot)
corrplot(PEARSON.r$R$r ,method = "circle")
corrplot(PEARSON.r$R$r ,method = "ellipse")

corrplot.mixed(PEARSON.r$R$r ,lower = "number",upper = "ellipse")


library(apaTables)
apa.cor.table(COR2, filename="Student_Grade_Prediction.doc")


#4. Create a visualization and answer the questions below
#which will provide an interesting story or insight within this data.

#Setup Working Directory
setwd("~/Downloads/02_IS 507/R files")
library(readr)
REG.raw <- read_csv("Student_Grade_Prediction.csv", col_names = TRUE)
MUREG <- REG.raw[c(24:30,32)]
str(MUREG) 

#run multiple regression
MUREG.OP <-lm ( G2 ~ famrel + freetime + goout + Dalc + Walc + health + absences, data= MUREG)
summary(MUREG.OP)

#run standardized multiple regression
library(lm.beta)
Std.MUREG.OP <-lm.beta(MUREG.OP)
summary(Std.MUREG.OP)


#check multi-collinearility
library(car)
vif(MUREG.OP)

#ggplot2
library(ggplot2)

ggplot(MUREG.OP, aes(x = famrel, y = G2)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP, aes(x = freetime, y = G2)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP, aes(x = goout, y = G2)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP, aes(x = Dalc, y = G2)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP, aes(x = Walc, y = G2)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP, aes(x = health, y = G2)) +
  geom_point(size=3) +stat_smooth(method="lm")

ggplot(MUREG.OP, aes(x = absences, y = G2)) +
  geom_point(size=3) +stat_smooth(method="lm")



