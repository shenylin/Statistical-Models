
library(foreign)
library(CCA)
library(yacca)
library(MASS)

#Read in Iris Data
setwd("insert working directory")

mmreg = read.csv("mmreg.csv", header = TRUE, sep = ",")
head(mmreg)

#See the first six lines of the data
head(mmreg)


###################################################################
# Exploring correlations between psychology and academics

#Create Sets of  Variables
psych = mmreg[, 1:3]
academic = mmreg[,4:8]

#Base Package
# This gives us the canonical correlates, but no significance tests
c = cancor(psych, academic)
c
####################################################################

#CCA library
library(CCA)

#Breakdown of the Correlations
matcor(psych, academic)

#Correlations between sepal and sepal (X)
#Correlations between petal and petal (Y)
cc_mm = cc(psych, academic)
cc_mm$cor

#Funcrions for CCA
ls(cc_mm)

#XCoef Correlations
cc_mm$xcoef

#YCoef Correlations
cc_mm$ycoef

#Calculate Scores
loadings_mm = comput(psych, academic, cc_mm)
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
c2 = cca(psych,academic)
c2

#CV1
helio.plot(c2, cv=1, x.name="Psych Values", 
           y.name="Academic Values")

#CV2
helio.plot(c2, cv=2, x.name="Psych Values", 
           y.name="Academic Values")

#Function Names
ls(c2)

# Perform a chi-square test on C2
c2
ls(c2)
c2$chisq
c2$df
summary(c2)
round(pchisq(c2$chisq, c2$df, lower.tail=F), 3)