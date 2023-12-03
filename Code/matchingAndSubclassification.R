# Topic 7

# load libraries

library(dagitty)
library(ggdag)
library(ggplot2)
library(MatchIt)
library(tidyverse)

# load data
df <- readRDS('Causal_Data_Science_Data/membership.rds')
summary(df)

# 1)

# I think that age and sex don't have an influence on the membership, but 
# have an influence on the avg_purchase and previous average purchase, as
# the previous purchase has an influence on the current purchase.
# Furthermore the previous average purchase is the only variable, which should
# have an influence in the membership, as people are more likely to sign the 
# membership if they are invested in the online shop.

dag_model <- 'dag {
bb="0,0,1,1"
membership [exposure,pos="0.075,0.4"]
avg_purchase [outcome,pos="0.4,0.4"]
sex [pos="0.3,0.6"]
age [pos="0.3,0.7"]
prev_avg_purchase [pos="0.2,0.65"]
membership -> avg_purchase
prev_avg_purchase -> avg_purchase
prev_avg_purchase -> membership
sex -> prev_avg_purchase
age -> prev_avg_purchase
sex -> avg_purchase
age -> avg_purchase
}
'

# draw DAG
ggdag_status(dag_model) +
  guides(fill = "none", color = "none")+
  theme_dag()+
  geom_dag_text(color = "black")



# 2)

summary(lm(avg_purch ~ card, df))

# If we compute the naive estimate of the average treatment effect
# we get an value of 25.2195. However I suspect that this value is not correct
# as I assumed that the average purchase is not only on the membership dependent

# 3)

# 3.1) (Coarsened) Exact Matching

cem <- matchit(card ~ sex+ age + pre_avg_purch,
               data = df, 
               method = 'cem', 
               estimand = 'ATE')

summary(cem)

df_cem <- match.data(cem)

model_cem <- lm(avg_purch ~ card, data = df_cem, weights = weights)
summary(model_cem)

# We can see that the ATE is around 10 smaller, than what the naive estimate was.

# 3.2)


nn <- matchit(card ~ sex+ age + pre_avg_purch,
               data = df, 
               method = 'nearest', 
               distance = 'mahalanobis',
               replace = T)
summary(nn)

df_nn <- match.data(nn)

model_nn <- lm(avg_purch ~ card, data = df_nn, weights = weights)
summary(model_nn)

# The ATE is very similar to the one obtained by CEM.
# This means both methods are valid



# 3.3) Inverse probability weighting

# (1) Propensity scores
model_prop <- glm(card ~ sex + age + pre_avg_purch,
                  data = df,
                  family = binomial(link = "logit"))
summary(model_prop)

# Add propensities to table
df_aug <- df %>% mutate(propensity = predict(model_prop, type = "response"))

# Extend data by IPW scores
df_ipw <- df_aug %>% mutate(
  ipw = (card/propensity) + ((1-card) / (1-propensity)))

# Look at data with IPW scores
df_ipw %>% 
  select(card, age, sex, pre_avg_purch, propensity, ipw)

model_ipw <- lm(avg_purch ~ card,
                data = df_ipw,
                weights = ipw)
summary(model_ipw)

# The estimated value is 14.9573, which fits with the other two models

# Looking for observations with high weights
df_ipw %>% 
  select(card, age, sex, pre_avg_purch, propensity, ipw) %>% 
  arrange(desc(ipw))

# Filter out extreme weights
model_ipw_trim <- lm(avg_purch ~ card,
                     data = df_ipw %>% filter(propensity %>% between(0.15, 0.85)),
                     weights = ipw)
summary(model_ipw_trim)

# The trim did not change anything as there are no extreme weights in our dataset