#IS 507 Assignment 1: Probability
#Due by Sun. Sep 12

#1 Educational attainment of couples.
#1-a
0.16+0.09
#1-b
0.17+0.09
#1-c
(0.16+0.09)*(0.17+0.09)


#2 rolling 2 dice 
#2-a
#There will be 1+4, 2+3, 3+2, 4+1, four sets of sum = 5
4/36
#Therefore, 1- 4/36 is the sum of the dice is not 5
1 - 4/36
#2-b 
#p(sum<7) = 15/36
#p(sum>=7) = 1- p(sum<7) = 21/36 
21/36
#2-c
#p(sum<=8) = 26/36
26/36

#3 Health coverage, relative frequencies
#3-b
23.29%
#3-c
20.99%
#3-d
2.3%


#4
#4-b-1   P(A and B)
0.3*0.7
#4-b-2   P(A or B)
0.3+0.7-0.21
#4-b-3   P(A|B)=[P(A)*P(B)]/P(B)
0.21/0.7
#4-d
#P(A|B)=[P(A)*P(B)]/P(B)
0.1/0.7

#5 Area under the curve
#Mean = 0
#Standard Deviation = 1
#5-a 
#p(Z>-1.13)
pnorm(x>-1.13, mean = 0, sd = 1)
#5-b 
#p(Z<0.18)
pnorm(x<0.18, mean = 0, sd = 1)
#5-c
#p(Z>8)
pnorm(x>8, mean = 0, sd = 1)
#5-d
#p(-0.5<Z<0.5)
round(1 - 2 * (pnorm(-0.5)),2)


#6 GRE scores
#6-a short-hand for these two normal distribution
#Verbal => N (????= 151,  ????= 7)
#Quantitative => N (????= 153,  ????= 7.67)
#6-b Sophia's Z-score on Verbal & Quantitative
#Verbal score = 160
#Mean Verbal Score = 151
#Standard Deviation Verbal Score = 7
(160-151)/7
#Quant score = 157
#Mean Quant Score = 153
#Standard Deviation Quant Score = 7.67
(157-153)/7.67
#6-c
#z-score tells us about where does Sophia's score percentile point in the test
#6-d => Verbal => Verbal Z-score is larger than Quant's
#6-e percentile scores
pnorm(160, mean = 151, sd = 7)
pnorm(157, mean = 153, sd = 7.67)
#6-f
1 - pnorm(160, mean = 151, sd = 7)
1 - pnorm(157, mean = 153, sd = 7.67)
#6-g
#Because only looking at raw scores may not give us the sense of where does the 
#scores stands in the normal distribution. We need to consider it in percentile
#sense so we know how strong is the scores compared to other test takers.
#6-h
#Yes. The calculation heavily relied on mean. However, if this is not a normal
#distribution, it means the mean is skewed.





