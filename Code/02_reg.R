#Topic 3 assignment

#load libraries
library(tinyverse)


# 1)

df <- readRDS('Causal_Data_Science_Data/car_prices.rds')
dim(df)

# Number of rows 181
# Number of columns 22

# 2)

summary(df)
glimpse(df)

# The data contains data of type double <dbl> and character <chr>.
# The data of type double are numeric variables, therefor it is possible to
# perform mathematical operations with them, which is also done during summary().
# However for factor variables (data of type character) it only counts the 
# occurrences.

# 3)

# All data
lmCarModelAll <- lm(price ~ ., data = df)
summary(lmCarModelAll)
sprintf("Adjusted R^2: %.4f", broom::glance(lmCarModelAll)$adj.r.squared)

# Data only significant with at least alpha <= 0.1
lmCarModell_zero_one <- lm(price ~ aspiration + doornumber + carbody + enginelocation
                           + carwidth + enginetype + cylindernumber+ enginesize
                           + fuelsystem  + stroke + peakrpm, data = df)
summary(lmCarModell_zero_one)
sprintf("Adjusted R^2: %.4f", broom::glance(lmCarModell_zero_one)$adj.r.squared)

# Data only significant with at least alpha <= 0.01
lmCarModell_zero_zero_one <- lm(price ~ aspiration + carbody + enginelocation
                           + carwidth + enginetype + cylindernumber+ enginesize
                           + stroke + peakrpm, data = df)
summary(lmCarModell_zero_zero_one)
sprintf("Adjusted R^2: %.4f", broom::glance(lmCarModell_zero_zero_one)$adj.r.squared)

# Data only significant with at least alpha <= 0.01 and no enginetype
lmCarModell_selected <- lm(price ~ aspiration + carbody + enginelocation
                                + carwidth + cylindernumber+ enginesize
                                + stroke + peakrpm, data = df)
summary(lmCarModell_selected)
sprintf("Adjusted R^2: %.4f", broom::glance(lmCarModell_selected)$adj.r.squared)

# Data only significant with at least alpha <= 0.1 and no doornumber
lmCarModell_noDoor <- lm(price ~ aspiration + carbody + enginelocation
                           + carwidth + enginetype + cylindernumber+ enginesize
                           + fuelsystem  + stroke + peakrpm, data = df)
summary(lmCarModell_noDoor)
sprintf("Adjusted R^2: %.4f", broom::glance(lmCarModell_noDoor)$adj.r.squared)

# After I tried a few linear regression models, where I started with all variables
# I got a adjusted R^2 of 0.9264. Then I started excluding variables, with a small
# significants. The best results were achived with following variables:
# aspiration, carbody, enginelocation, carwidth, enginetype
# cylindernumber, enginesize, fuelsystem, stroke and peakrpm
#
# With this variables I was able to achieve a adjusted R^2 of 0.928

# 4)

# 4.1)
#  I choose enginesize

# enginesize is of the type <dbl>, which means it contains numeric values,
# which can have decimal points. However enginesize is in the dataframe only 
# represented by integer values. Also it would make no sense for it to be negative

# 4.2)
# Plot relationship between enginesize and price
ggplot(df, aes(x = enginesize, y = price)) +
  geom_point(alpha = 0.8)

# If you plot the enginesize in regard to the prize you can see that a bigger
# engine size correlates to an higher price and this correlation seems to be linear.
# So increasing the engine size will, most likely result in a higher price and a
# smaller engine size in a lower price

# 4.3)

summary(lmCarModell_noDoor)

# If you take a look at the before found linear model, you can see that the
# engine size is statistical significant with an p-value less than an alpha = 0.001.
# The same can be seen by the linear model without variable selection.

# 5)

mutatedDf <- df %>% mutate(seat_heating = TRUE)

lmCarModelMutated <- lm(price ~ ., data = mutatedDf)
summary(lmCarModelMutated)
sprintf("Adjusted R^2: %.4f", broom::glance(lmCarModelMutated)$adj.r.squared)

# The coefficient of the new variable is NA, which stands for "not available".
# This result is explained with the fact that seat_heating is true for all
# prices, which creates singularities, as it has absolutely no impact on the price


