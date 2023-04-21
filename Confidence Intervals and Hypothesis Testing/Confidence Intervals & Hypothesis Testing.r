#Confidence Intervals & Hypothesis Testing

#Proportion Confidence Intervals

#1-Proportion Hypothesis Testing for 2-Tailed Test using Z-Test

#Use Z-Scores


#P(Z<-2.20)

pnorm(-2.20)

#Or Use the complement for larger than


###############################################################################


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


#If Missing data, use listwise deletion.
#stress_echo2 <- na.omit(stress_echo)

#Impute data using the mice package in R

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


stress_echo$age_binary <-cut(stress_echo$age, c(26, 65, 93), include.lowest = TRUE)

table(stress_echo$age_binary)

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


#Check Descriptive Statistics (N NMiss Mean SD Q1 Median Q3 Skewness Kurtosis)

library(psych)

describe(stress_echo)

library(Hmisc)

describe(stress_echo)

summary(stress_echo$bhr)
################################################################################

#1-Proportional test

table(stress_echo$death)

24+534

death <- prop.test(x = 24, n = 558, p = 0.50, 
                 correct = FALSE)

death

#X2 = (Z)^2
#sqrt(X2 test statistic) = Z-Score

################################################################################
#2-Sample Confidence Interval

prop.test(table(stress_echo$gender_num, stress_echo$death), correct=FALSE) 

################################################################################

#Chi-Square Test of Association

#Research Question:  Is there a relationship between hxofCig and hxofMI?

library(gmodels)

CrossTable(stress_echo$hxofCig,stress_echo$hxofMI,digits = 2, expected=TRUE, prop.r=TRUE, 
           prop.c = TRUE, prop.chisq = FALSE, chisq = TRUE, fisher = TRUE, format="SPSS")

###############################################################################
#Research Question: Is there a change in diagnosis between baseline and end diagnosis for MI?

#McNemar Test

library(gmodels)

CrossTable(stress_echo$hxofMI,stress_echo$newMI,digits = 2, prop.r=TRUE, 
           prop.c = TRUE, format="SPSS")


mcnemar.test(stress_echo$hxofMI,stress_echo$newMI)
################################################################################
#Research Question:  Is there a difference between genders for Baseline Systolic Blood Pressure?


#Check Normality Visually Looking at Boxplots

library(RVAideMemoire)

byf.shapiro(as.matrix(stress_echo$basebp)~gender_num,data=stress_echo)


#What doe the normality tests tell us?


#What is the appropriate test to use?


#Independent (Student) T-test

t.test(stress_echo$basebp~stress_echo$gender_num) # where y is numeric and x is a binary factor


#Mann-Whitney U (Wilcoxon) test - Nonparametric T-Test

wilcox.test(stress_echo$basebp~stress_echo$gender_num) # where y is numeric and A is A binary factor

#Reporting medians (interquartile ranges - 25% percentile - 75% percentile)

library(Hmisc)

with(stress_echo, tapply(basebp, gender_num, describe))

#For Females, 136.0 (122.2-150.0)
#For Males, 130.0 (120.0-145.0)
################################################################################
#Research Question:  Is there a difference between baselineEF and dobEF?

#Paired T-Test

t.test(stress_echo$baseEF, stress_echo$dobEF, paired=TRUE) # where y1 & y2 are numeric

#Paired Test - Nonparametric (Sign Rank test)

wilcox.test(stress_echo$baseEF, stress_echo$dobEF, paired=TRUE) # where y1 and y2 are numeric

library(Hmisc)

describe(stress_echo$baseEF)

describe(stress_echo$dobEF)

#What does the Sign Rank test tell you?
################################################################################
