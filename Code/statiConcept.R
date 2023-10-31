#Topic 2
# load necessary data
random_vars <- readRDS("Causal_Data_Science_Data/random_vars.rds")
library(tidyverse)

# sort the data by age

sorted_vars_age <- arrange(random_vars,age)
View(sorted_vars_age)

# 1)
# age
# expected value
groupedAge <- sorted_vars_age %>% group_by(age) %>% summarise( n = n())
expectedAge <- weighted.mean(groupedAge[,1],groupedAge[,2])

# variance
varianceAge <- var(groupedAge[[1]])

# standard deviation
deviationAge <- sd(groupedAge[[1]])

# income
# expected value
groupedIncome <- sorted_vars_age %>% group_by(income) %>% summarise( n = n())
expectedIncome <- weighted.mean(groupedIncome[,1],groupedIncome[,2])

# variance
varianceIncome <- var(groupedIncome[[1]])

# standard deviation
deviationIncome <- sd(groupedIncome[[1]])

# 2)
# It doesn't make sense to compare the two standard deviations as they describe
# the variation of two different data sets, what would make more sense is to
# compare the standard deviations of the income of different age groups

# 3)
# covariance
covariance <- cov(sorted_vars_age[[1]],sorted_vars_age[[2]])

# correlation
correlation <- cor(sorted_vars_age[[1]],sorted_vars_age[[2]])

# 4)
# The correlation is easier to interpret, as it is normalized in respect to the
# variance of the two data sets.
# From the fundamentals we learned that a high covariance means a low independence,
# however because of the scaling of the data sets we can't define what high means.
# 
# Here comes the correlation into play as it norms the covariance to a value of
# -1 to 1, where values close to 1 mean a nearly linear relationship.
# In our data set we have a correlation of 0.548, which indicates that there exists
# a dependency between age and income and that with growing age the income also growths.
# However as 0.548 is still far away from 1 we can also conclude that there are many 
# more factors, which have an impact on the income, which we didn't consider.

# 5)
# age < 18
selectedAgeOne <- sorted_vars_age  %>% filter(age<18) %>% group_by(income) %>% summarise( n = n())
conAgeOne <- weighted.mean(selectedAgeOne[,1],selectedAgeOne[,2])

# age [18,65)
selectedAgeTwo <- sorted_vars_age  %>% filter(age>=18 & age<65) %>% group_by(income) %>% summarise( n = n())
conAgeTwo <- weighted.mean(selectedAgeTwo[,1],selectedAgeTwo[,2])

# age >= 65
selectedAgeThree <- sorted_vars_age  %>% filter(age>=65) %>% group_by(income) %>% summarise( n = n())
conAgeThree <- weighted.mean(selectedAgeThree[,1],selectedAgeThree[,2])
