# load libraries

library(tidyverse)
library(ggplot2)
library(rddensity)
library(ggthemr)
ggthemr('flat dark')


###################################### Coupons ###############################

# load data

df <- readRDS('Causal_Data_Science_Data/coupon.rds')
summary(df)

c0 <- 60

rddd <- rddensity(df$days_since_last, c = c0)
summary(rddd)

# Visually check continuity at running variable
rdd_plot <- rdplotdensity(rddd, df$days_since_last, plotN = 100)

# Specify bandwidth
bw <- c0 +c(-5,5)

# Subsets below and above threshold in specified bandwidth
df_bw_below <- df %>% filter(days_since_last >= bw[1] & days_since_last < c0)
df_bw_above <- df %>% filter(days_since_last >= c0 & days_since_last <= bw[2])

df_bw <- bind_rows(df_bw_above, df_bw_below)
dim(df_bw)

# Extract values for vertical lines to visualize local average treatment effect
model_bw_below <- lm(purchase_after ~ days_since_last, df_bw_below)
model_bw_above <- lm(purchase_after ~ days_since_last, df_bw_above)

y0 <- predict(model_bw_below, tibble(days_since_last = c0))
y1 <- predict(model_bw_above, tibble(days_since_last = c0))

late <- y1 - y0
sprintf("LATE: %.2f", late)

# Compute coefficients for specified bandwidth.
lm_bw <- lm(purchase_after ~ days_since_last_centered + coupon, df_bw)
summary(lm_bw)


########################## Half bandwidth ########################

# Specify bandwidth
bwh <- c0 +c(-2.5,2.5)

# Subsets below and above threshold in specified bandwidth
df_bw_below <- df %>% filter(days_since_last >= bwh[1] & days_since_last < c0)
df_bw_above <- df %>% filter(days_since_last >= c0 & days_since_last <= bwh[2])

df_bw <- bind_rows(df_bw_above, df_bw_below)
dim(df_bw)

# Extract values for vertical lines to visualize local average treatment effect
model_bw_below <- lm(purchase_after ~ days_since_last, df_bw_below)
model_bw_above <- lm(purchase_after ~ days_since_last, df_bw_above)

y0 <- predict(model_bw_below, tibble(days_since_last = c0))
y1 <- predict(model_bw_above, tibble(days_since_last = c0))

late <- y1 - y0
sprintf("LATE: %.2f", late)

# Compute coefficients for specified bandwidth.
lm_bw <- lm(purchase_after ~ days_since_last_centered + coupon, df_bw)
summary(lm_bw)

############################### double bandwidth ##########################

# Specify bandwidth
bwd <- c0 +c(-10,10)

# Subsets below and above threshold in specified bandwidth
df_bw_below <- df %>% filter(days_since_last >= bwd[1] & days_since_last < c0)
df_bw_above <- df %>% filter(days_since_last >= c0 & days_since_last <= bwd[2])

df_bw <- bind_rows(df_bw_above, df_bw_below)
dim(df_bw)

# Extract values for vertical lines to visualize local average treatment effect
model_bw_below <- lm(purchase_after ~ days_since_last, df_bw_below)
model_bw_above <- lm(purchase_after ~ days_since_last, df_bw_above)

y0 <- predict(model_bw_below, tibble(days_since_last = c0))
y1 <- predict(model_bw_above, tibble(days_since_last = c0))

late <- y1 - y0
sprintf("LATE: %.2f", late)

# Compute coefficients for specified bandwidth.
lm_bw <- lm(purchase_after ~ days_since_last_centered + coupon, df_bw)
summary(lm_bw)

######################### Results #################################

# The results seem to be relative sensitive to changing the bandwidth if you want
# to get the exact value of the LATE, however the variable days_since_last_centered
# lost its significance, therefore you can notice if you have the wrong bandwidth 

##############################################################################

########################### shipping ####################################

#load data

df <- readRDS("Causal_Data_Science_Data/shipping.rds")
summary(df)

# Just from the info alone, one can assume that the purchase amount is continuous
# and smooth. Therefore it should be a valid running variable.
# However if the value is known at which free shipping is offered, which normally
# is the case, one can expect a heaping at the cut-off. Then this is the case,
# the continuity assumption is no longer fulfilled, as the function is not smooth.
# 
# In contrast to the coupon, and the day since last purchase, which people can't
# manipulate. The purchase amount can be manipulated by the customer.

# For that reason I would argue that the purchase amount for free shipping is not a 
# valid running variable, as it can be manipulated by the customer and as a result,
# one would notice a heaping at the cut-off

# Manual plot
ggplot(df, aes(x = purchase_amount)) +
  geom_histogram(binwidth = 5, color = "white", boundary = 30, alpha = .6) +
  geom_vline(xintercept = 30, color = ggthemr::swatch()[5]) +
  xlab("Purchase amount")+
  ylab("Number of purchases")

# As expected one can see that the number of purchases is falling of sharply right
# before the 30 mark, which grants free shipping.
# This can be explained by the fact that the free shipping purchase amount is known
# before hand and customers are more willing to buy another small thing to get
# past that cut-off.

# Therefore this plot nicely illustrates that the purchases amount is not a suitable
# running variable, as the continuity assumption is not fulfilled.