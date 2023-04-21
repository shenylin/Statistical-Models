#Introduction to R

#shortcut to Run Data
#Highlight Syntax
#Hold Command + Enter

#Mathematical Operations

#Addition
3+4

#Substraction
8-5

#Multiplication
3*4

#Division
24/8

#Order of Operations
#Grouping Symbols
#Exponents
#Muktiplication & Division from Left to Right
#Addition & Subtraction from Left to Right

4+((3*2)^2)-1

#Note the carat will not always be used for exponents
#(i.e. not for matrix multiplication)

#create a vector
x<-c(1,3,5)
x

x = c(2,6,8)
x

#check length of a vector
length(x)

#Taking the Square Root of a Number
sqrt(36)

#Look at list of objects - temporary or permanent varieables/datasets created
#Also, for seeing functions available in a package

ls()


#Create a Sample of Coin Flips
#Assuming you are flipping the same coin nth times

sample(c("H","T"), 10, replace=TRUE)


#Calculate the Probability


#Binomial Distribution

#p(x<=7) of with a sample size of 10 and a probability of 0.50

round(pbinom(size = 10, prob = 0.5, q =7),2)


#How would you calculate with the formula?

#How would you solve p(x> 7)? Hint: complement

round(1 - pbinom(size = 10, prob = 0.5, q =7),2)


#Now, compute p(4<= x<=7)

round(pbinom(size = 10, prob = 0.5, q = 7) - pbinom(size = 10, prob = 0.5, q = 4),2)

#Alternative Method
round(diff(pbinom(c(4,7),size = 10, prob = 0.5)), 2)


#Showing Trials for Rolling a Dice
#Set seed for reproducibility
set.seed(1)

#rolling a dice ten times in a row
sample(1:6, 10, replace = TRUE)

#cards
sample(1:52, 10, replace = TRUE)

#Normal Distrubution

#ACT Score = 24
#Mean ACT Score = 28
#Standard Deviation ACT Score = 5

pnorm(24, mean = 28, sd = 5)
round(pnorm(24, mean = 28, sd = 5),2)

#Use Z-Scores

#p(Z<1.337)
pnorm(1.377)
round(pnorm(1.377),2)

#p(-1.96<= x<= 1.96)
#Within the 2nd standard deviation of mean
1 - 2 * (pnorm(-1.96))
round(1 - 2 * (pnorm(-1.96)),2)
#p(3<=x<=4)
#mean=5
#sd=5

pnorm(4, mean=5, sd=5) - pnorm(3, mean=5, sd=5)


#what if you know the probability and you want to work backwards to the z-score?

#use qnorm

round(qnorm(0.95),2)

#Now, you can back into solving for the observed value

#z-score = 1.64
#Average(m)=10
#Standard Deviation(sd) = 2

#x = z*(sd) + m

x = 1.64*(2) + 10
x


#Poisson Distribution

#Average = 5 cars go through a stop light per hour

#p(x<=10)
ppois(10, lambda = 5)
round(ppois(10, lambda = 5),2)
#p(x>11)
ppois(10, lambda = 5, lower=FALSE)
round(ppois(10, lambda = 5, lower=FALSE),2)






