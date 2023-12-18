# load libraries

library(tidyverse)
library(ggplot2)
library(rddensity)


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

###########################