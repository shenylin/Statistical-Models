##Assignment 2
##ChengHsuan Lin / 09192021


#Setup Working Directory

setwd("~/Downloads/02_IS 507/R files")

#Read in Data

library(readr)

student_grade_prediction <- read_csv("Student_Grade_Prediction.csv", col_names = TRUE)


#Check data is read in correctly
View(student_grade_prediction)
head(student_grade_prediction)

#Check Dimensions of Dataset
dim(student_grade_prediction)
#395 Observations 33 variables


#Check Variable Names
names(student_grade_prediction)


#Check Data structure
str(student_grade_prediction)


#Check Missing Data
sum(is.na(student_grade_prediction))
#No Missing Data

############################

#1. Is there a difference for G3-final grades for these Portuguese students between students with internet from students without internet?
#Need to change internet variable to read as numeric variable instead of categorical variable
library(plyr)
#internet
table(student_grade_prediction$internet)
student_grade_prediction$internet_num <- revalue(student_grade_prediction$internet, c("yes"="1", "no"="0"))
student_grade_prediction$internet_num <- as.numeric(student_grade_prediction$internet_num)

#Research Question: Is there a difference for G3-final grades for these Portuguese students between students with internet from students without internet?
#Check Normality Visually Looking at Boxplots

library(RVAideMemoire)
byf.shapiro(as.matrix(student_grade_prediction$G3)~internet_num,data=student_grade_prediction)


#Mann-Whitney U (Wilcoxon) test - Nonparametric T-Test (data not normal)
wilcox.test(student_grade_prediction$G3 ~ student_grade_prediction$internet_num) 
# where y is numeric and A is A binary factor


#############################

#2. Is there a relationship between activities and higher education? 
#higher education => variable: higher

#Need to change activities and higher variables to read as numeric variables instead of categorical variables
library(plyr)

#activities
table(student_grade_prediction$activities)
student_grade_prediction$activities_num <- revalue(student_grade_prediction$activities, c("yes"="1", "no"="0"))
student_grade_prediction$activities_num <- as.numeric(student_grade_prediction$activities_num)

#higher
table(student_grade_prediction$higher)
student_grade_prediction$higher_num <- revalue(student_grade_prediction$higher, c("yes"="1", "no"="0"))
student_grade_prediction$higher_num <- as.numeric(student_grade_prediction$higher_num)

#Chi-Square Test of Association
#Research Question:Is there a relationship between activities and higher education?
library(gmodels)
CrossTable(student_grade_prediction$activities,student_grade_prediction$higher,digits = 2, expected=TRUE, prop.r=TRUE, 
           prop.c = TRUE, prop.chisq = FALSE, chisq = TRUE, fisher = TRUE, format="SPSS")

######################################3
#3. Is there a difference between the G2-second grade and the G3-final grade?
#Research Question: Is there a difference between the G2-second grade and the G3-final grade?
#Paired T-Test
t.test(student_grade_prediction$G2, student_grade_prediction$G3, paired=TRUE) 
# where             y1              &         y2 are numeric
library(Hmisc)

describe(student_grade_prediction$G2)

describe(student_grade_prediction$G3)

################

#4. Create two categorical variables for G2-second grade and the G3-final grade using a cutoff of 15. 
#Test whether there is a difference between the two categorical variables. 
#Hint: Remember what these categorical variables represent.

###G2-second grade
#Create a grade Categorical Variable using a cutoff of 15
#Use the cutoff points 15 and 19 in R

student_grade_prediction$G2<-cut(student_grade_prediction$G2,2)
#(0,15] (15,19]

table(student_grade_prediction$G2)

G2 <- 0:19
student_grade_prediction$G2 <-cut(student_grade_prediction$G2, c(0, 15, 19), include.lowest = TRUE)

table(student_grade_prediction$G2)

student_grade_prediction$G2<-NULL
student_grade_prediction$G2[(student_grade_prediction$G2 >= 0) & (student_grade_prediction$G2 <= 15)] <- "grade LT 16"
student_grade_prediction$G2[(student_grade_prediction$G2 > 15) & (student_grade_prediction$G2 <= 19)] <- "grade Between 16 and 19"

student_grade_prediction$G2<-NULL
student_grade_prediction$G2[(student_grade_prediction$G2 >= 0) & (student_grade_prediction$G2 <= 15)] <- 0
student_grade_prediction$G2[(student_grade_prediction$G2 > 15) & (student_grade_prediction$G2 <= 19)] <- 1


table(student_grade_prediction$G2)


###G3-second grade
#Create a grade Categorical Variable using a cutoff of 15
#Use the cutoff points 15 and 20 in R

student_grade_prediction$G3<-cut(student_grade_prediction$G3,4)
#(0,15] (15,20]

table(student_grade_prediction$G3)

G3 <- 0:20
student_grade_prediction$G3 <-cut(student_grade_prediction$G3, c(0, 15, 20), include.lowest = TRUE)

table(student_grade_prediction$G3)

student_grade_prediction$G3<-NULL
student_grade_prediction$G3[(student_grade_prediction$G3 >= 0) & (student_grade_prediction$G3 <= 15)] <- "grade LT 16"
student_grade_prediction$G3[(student_grade_prediction$G3 > 15) & (student_grade_prediction$G3 <= 20)] <- "grade Between 16 and 20"

student_grade_prediction$G2<-NULL
student_grade_prediction$G2[(student_grade_prediction$G2 >= 0) & (student_grade_prediction$G2 <= 15)] <- 0
student_grade_prediction$G2[(student_grade_prediction$G2 > 15) & (student_grade_prediction$G2 <= 20)] <- 1

table(student_grade_prediction$G3)

########

#5. What is the proportion of students receiving extra educational support?

#Proportional test

table(student_grade_prediction$schoolsup)

344+51

schoolsup <- prop.test(x = 51, n = 395, p = 0.50, 
                   correct = FALSE)

schoolsup

############
#6. What is the proportion of males and females in extracurricular activities?

table(student_grade_prediction$sex)

208+187

activities <- prop.test(x = c(96, 105), n = c(208, 187))

activities




END--------------09192021

