# Load the traffic data
traffic <- read.csv("./Data/traffic.csv")

# Part A
# Calculate correlation matrix and create a scatterplot matrix
cor(traffic)
pairs(traffic)

# Part B
# Fit a linear regression model
fit <- lm(spi ~ ., data = traffic)
summary(fit)

# Calculate confidence interval for the 'weather' variable
confint(fit, 'weather', level = 0.95)

# Part C
# Display the summary of the linear regression model
summary(fit)

# Present the model equation
# spi = 62.8071 - 2.1750 * transport - 2.4097 * road + 4.2456 * weather - 3.6145 * fuel - 0.1358 * wind

# Null hypothesis: Beta1 = Beta2 = ... = BetaN = 0
# Alternate hypothesis: At least 1 Beta(i) != 0

# Anova table for model comparison
traffic$null <- mean(traffic$spi)
null_fit <- lm(spi ~ null, data = traffic)
summary(null_fit)
anova(null_fit, fit)

# Part D
# Goodness of fit
summary(fit)

# Analyze residuals
plot(fit)
# Check for normal distribution, homoscedasticity, and independence

# Part E
# R-squared is a measure of the model fit and explained variability
summary(fit)

# Part F (Stepwise Selection)
# Forward stepwise selection
fit <- lm(spi ~ transport + road + weather + fuel, data = traffic)
summary(fit)

# Backward stepwise selection (removing 'fuel')
fit <- lm(spi ~ transport + road + weather, data = traffic)
summary(fit)

# Part G
# Adjusted R^2 takes into account the number of variables in the dataset
