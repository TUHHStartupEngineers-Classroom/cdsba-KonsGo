# Assignment 1
rm(list = ls())

scope = 0.3;
nScope = 0.7;

timeScope = 0.2;
nTimeScope = 0.8;

timeNScope = 0.6;
nTimeNScope = 0.4;

p1 = scope * timeScope
p2 = scope * nTimeScope
p3 = nScope * timeNScope
p4 = nScope * nTimeNScope

sum = p1+p2+p3+p4

# Assignment 2

# All three = 0.5% or 5 people
# At least two = 8.8% + 7.3% + 3.3% => 19.4% or 194 people
# only one = 10% + 27.8% + 42.3% => 80.1% or 801 people

# Assignment 3 

pBA = 0.97
pBnA = 0.01
pA = 0.04
pnA = 0.96

pnAB = (pBnA*pnA)/(pBA*pA+pBnA*pnA)

pAB = (pBA*pA)/(pBA*pA+pBnA*pnA)

pges = pnAB + pAB

# These results show that in case the alarm is triggered, 
# there is a possibility of about pnAB% that the product is flawless 
# and a probability of pAB% that the product is faulty.

