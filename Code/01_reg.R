# Topic 3 lecture

# Load tidyverse package
library(tidyverse)

ggthemr("flat dark", set_theme = FALSE)

# Simulate data

# number of observations
n <- 10

# Create tibble
lm_dat <- tibble(
  # draw from normal distribution
  x = rnorm(n, mean = 3, sd = 1),
  # y depends on x and noise from normal distribution
  y = 0.3*x + rnorm(n, 0, 0.2)
)

# Show data
lm_dat

# Scatter plot of x and y
ggplot(lm_dat, aes(x = x, y = y)) + 
  geom_point(size = 3, alpha = 0.8) # change size and opacity of points

# Fit model and print summary
lm_mod <- lm(y ~ x, lm_dat)
summary(lm_mod)

# Add fitted values and residuals to data
lm_dat_fit <- lm_dat %>% 
  mutate(y_fit = predict(lm_mod),
         r   = y - y_fit)

# Plot distance of actual to fit
ggplot(lm_dat_fit, aes(x = x, y = y)) + 
  geom_point(size = 3) +
  geom_smooth(method='lm', formula= y ~ x, se = F) +
  geom_segment(aes(xend = x, yend = y_fit), color = ggthemr::swatch()[2]) +
  labs(title = "Predicted observations vs actual observations")
# Plot residuals
ggplot(lm_dat_fit, aes(x = x, y = r)) +
  geom_point(size = 3) +
  geom_smooth(method='lm', formula= y ~ x, se = F) +
  geom_segment(aes(xend = x, yend = 0), color = ggthemr::swatch()[2]) +
  labs(title = "Residuals vs zero")

# Next steps
# Read data set. Check what folder/path your data is in. 
df <- readRDS("Causal_Data_Science_Data/health_ins.rds")

# Show first lines
head(df)

# Show summary statistics: various types of variables (continuous, discrete,
# binary/logical, categorical/factor)
summary(df, digits = 3)

# Show correlation
df %>%
  select(-region) %>% 
  cor() %>% 
  round(2) %>% 
  Matrix::tril()

# Include all potential regressors
lm_all <- lm(expected_cost ~ ., data = df)
summary(lm_all)

# Show CIs at different levels of alpha
# alpha = 0.05
confint(lm_all, level = 0.95)
# alpha = 0.1
confint(lm_all, level = 0.90)

# Plot histogram of residuals
ggplot(tibble(res = lm_all$residuals), aes(x = res)) + 
  geom_histogram(color="white", alpha = 0.8, binwidth = 30) +
  labs(x = "residuals", y = "frequency")

# Include only significant regressors
lm_imp <- lm(expected_cost ~ age + bmi + smoking, data = df)
summary(lm_imp)

# Compare R^2
sprintf("Adjusted R^2: %.2f", broom::glance(lm_all)$adj.r.squared)
sprintf("Adjusted R^2: %.2f", broom::glance(lm_imp)$adj.r.squared)

# AIC
sprintf("AIC: %.2f", AIC(lm_all))
sprintf("AIC: %.2f", AIC(lm_imp))

# Plot relationship between BMI and expected cost
ggplot(df, aes(x = bmi, y = expected_cost)) +
  geom_point(alpha = 0.8)

# Include quadratic term for BMI
lm_sq <- lm(expected_cost ~ age + bmi + I(bmi^2) + smoking, data = df)
summary(lm_sq)