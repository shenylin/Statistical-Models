#Assignment 5
#Cheng-Hsuan Lin
#Date: Oct 31, 2021
#Principal Component Analysis (PCA) and Factor Analysis in R
#Using BIG5.csv

#Libraries
library(DescTools)
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations
##############################################################################################

#Set Working Directory
setwd("~/Downloads/02_IS 507/R files")

#Read in Datasets
Personality <- read.csv(file="BIG5.csv", header=TRUE, sep=",")

#Check Sample Size and Number of Variables
dim(Personality)
#19,719 Sample Size and 50 variables

#Show for first 6 rows of data
head(Personality)
names(Personality)

################################################################################################

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(Personality))
#0 missing values 


#################################################################################################################

#Show Structure of Dataset
str(Personality, list.len=ncol(Personality))
str(Personality)

#Show column Numbers
names(Personality)

#Show descriptive statistics
library(psych)
describe(Personality)
#Normality Rule of Thumb with Skewnewss and Kurtosis (think normal bell curve):
#Short Way:
#If skewnewss is close to 0, the distribution is normal.
#If Kurtosis is -3 or 3, the distribution is normal.

#If skewness is less than -1 or greater than 1, the distribution is highly skewed.
#If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed.
#If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.

#Test of Contradiction

#Ho:  Data is Normal (p > .05)
#Ha:  Data is Not Normal (p < .05)


##################################################
#PCA/FA
#Test KMO Sampling Adequacy
library(psych)
KMO(Personality)
#Overall MSA =  0.91

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(Personality)
#p-value < 2.22e-16 (Very Small Number)

#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(Personality, check.keys = TRUE)
#raw_alpha = 0.88

library(fmsb)
CronbachAlpha(Personality)
#Cronbach???s alpha = 0.55 #acceptable

#######################################################
#Parallel Analysis (Horn's parallel analysis)

#Created a Psychologist John L. Horn in 1965

#Closest to Heuristic Determination of Number of Components or Factors

#Compares actual eigenvalues with ones from a Monto-Carlo simulated dataset of
#the same size

#Dependent upon sample size, correlation coefficient, and how items fall on 
#components

library(psych)

comp <- fa.parallel(Personality)
comp

#######################################################
#Create PCA
p = prcomp(Personality, center=T, scale=T)
p

#Check Scree Plot
plot(p)
abline(1, 0)

#Check PCA Summary Information
summary(p)
print(p)

########################################################

# The Psych package has a wonderful PCA function that allows many more options
# including build-in factor rotation, specifying a number of factors to include 
# and automatic "score" generation

#Best Way to Conduct PCA Analysis
p2 = psych::principal(Personality, rotate="varimax", nfactors=5, scores=TRUE)
p2

p3<-print(p2$loadings, cutoff=.4, sort=T)


#PCAs Other Available Information

ls(p2)

p2$values
p2$communality
p2$rot.mat

########################################################################################

#Calculating scores

scores <- p2$scores
cor(scores)

summary(scores)

scores_1 <- scores[,1]

min_score <- min(scores_1)
min_score

max_score <- max(scores_1)
max_score

summary(scores_1)

scores_2 <- scores[,2]
scores_3 <- scores[,3]


#Conducting Factor Analysis
fit = factanal(Personality, 5)
print(fit$loadings, cutoff=.5, sort=T)

#######################################################################################
#Additional PCA Visualizations

#Using Factoextra
library(factoextra)

p3 <- prcomp(Personality, scale = TRUE) 
fviz_eig(p3)

#PCA Individuals
pI<-fviz_pca_ind(p3,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
pI

#PCA Variables
pca_var<-fviz_pca_var(p3,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pca_var

#Biplot
bi_plot<-fviz_pca_biplot(p3, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

bi_plot

library("FactoMineR")
p4 <- PCA(Personality, graph = FALSE)
#IF graph is set to true, it will provide the individual and variable maps

#Shows all the objects or functions available in PCA
print(p4)

#Options for providing screeplot
fviz_eig(p4, addlabels = TRUE, ylim = c(0, 35))
fviz_screeplot(p4, addlabels = TRUE, ylim = c(0, 35))

variables <- get_pca_var(p4)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables$contrib, 11)

library("corrplot")
corrplot(variables$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(p4, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(p4, choice = "var", axes = 2, top = 10)


library(ade4)
p5 <- dudi.pca(music2,
               scannf = FALSE,   # Hide scree plot
               nf = 3          # Number of components kept in the results
)
fviz_screeplot(p5, addlabels = TRUE, ylim = c(0, 35))

variables2 <- get_pca_var(p5)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables2$contrib, 11)

library("corrplot")
corrplot(variables2$contrib, is.corr=FALSE)    

