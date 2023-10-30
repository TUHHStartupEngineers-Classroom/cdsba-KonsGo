# load necessary data
random_vars <- readRDS("Causal_Data_Science_Data/random_vars.rds")
library(tidyverse)

# sort the data by age

sorted_vars_age <- arrange(random_vars,age)
# View(sorted_vars_age)

# 