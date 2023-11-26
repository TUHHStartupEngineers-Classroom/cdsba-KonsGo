# Topic 5

 
# Load packages
library(dagitty)
library(ggdag)
library(ggplot2)

# create DAG from dagitty
dag_model <- 'dag {
bb="0,0,1,1"
D [exposure,pos="0.075,0.4"]
Y [outcome,pos="0.4,0.4"]
Z1 [pos="0.2,0.2"]
Z2 [pos="0.3,0.5"]
Z3 [pos="0.2,0.6"]
Z4 [pos="0.4,0.6"]
D -> Y
D -> Z3
Z1 -> D
Z1 -> Y
Z2 -> Y
Z2 -> Z3
Z3 -> Z4
}
'
# draw DAG
ggdag_status(dag_model) +
  guides(fill = "none", color = "none")  # Disable the legend

# Part 1

parking_spot <- 'dag{
bb="0,0,1,1"
spots [exposure,pos="0.3,0.4"]
sales [outcome,pos="0.4,0.4"]
location [pos="0.35,0.5"]
spots -> sales
location -> sales
location -> spots
}
'

ggdag_status(parking_spot) +
  guides(fill = "none", color = "none")+
  theme_dag()+
  geom_dag_text(color = "black")

# part 2

df <- readRDS("Causal_Data_Science_Data/customer_sat.rds")

lm_followUp <- lm(satisfaction ~ follow_ups, data = df)
lm_follAndSub <- lm(satisfaction ~ follow_ups + subscription, data = df)

# part 3

summary(lm_followUp) # m1 = -3.3093 b = 78.886

summary (lm_follAndSub) # m1 = 2.1944 m2 = 44.7222 m3 = 18.0722 b = 26.7667

# Explanation: As the satisfaction decreases, if the data is not conditioned 
# on subscription, but at the same time increases, if conditioned on subscription
# you can assume that the subscription has an negative impact on the satisfaction
# This can have multiple explanations, maybe the product is to expensive or
# the amount of functions to overwhelming. 

# part 4

# Not conditioning on subscription
simps_not_cond <- ggplot(df, aes(x = follow_ups, y = satisfaction)) +
  geom_point(alpha = .8) +
  stat_smooth(method = "lm", se = F)

# Conditioning on subscription
simps_cond <- ggplot(df, aes(x = follow_ups, y = satisfaction, color = subscription)) +
  geom_point(alpha = .8) +
  stat_smooth(method = "lm", se = F) +
  theme(legend.position = "right")

# Plot both plots
simps_not_cond
simps_cond
