
setwd("/Volumes/STORE N GO/DSA211/Homework/HW5")
business <- read.csv("business.csv")
View(business)
names(business)
summary(business)
# pairs(business)

# (a),(b),(c)
lm.fit <- lm(PB.Ratio~., data = business)
summary(lm.fit)

# (d)
lm.fit2 <- lm(PB.Ratio~ROE+Local, data = business)
summary(lm.fit2)

# (e)
lm.fit3 <- lm(PB.Ratio~ROE*Local, data = business)
summary(lm.fit3)

# (g)
attach(business)
plot(lm.fit3$fitted.values, residuals(lm.fit3),
     main = "Relationship between predicted price-to-book value and residuals",
     xlab = "predicted price-to-book", ylab = "residuals")
plot(ROE, residuals(lm.fit3), main = "Relationship between ROE and residuals",
     xlab = "ROE", ylab = "Residuals")


library(fitdistrplus)
fnorm <- fitdist(residuals(lm.fit3), "norm")
summary(fnorm)
plot(fnorm)


# (h)
predict(lm.fit3, data.frame(Local="Yes", ROE=24.1))

# (i)
predict(lm.fit3, data.frame(ROE=24.1, Local="No"), interval = "prediction", level = 0.95)

# (j)
lm.fit3
# To find the intercept
lm.fit3$coefficients[1] + lm.fit3$coefficients[3]
# To find the gradient of ROE
lm.fit3$coefficients[2] + lm.fit3$coefficients[4]

# ANSWERS of Q1
# (a) 
# PB.Ratio = 5.67  + (0.04538)ROE + (0.0003645)SGrowth + (0.4777)LocalYes + (0.1040)SizeMedium + (0.1361)SizeSmall

# (b)
# Since p-value = 2.2e-16, there is a significant linear relationship.

# (c)
# only ROE and Local make a significant contribution to the regression model at 0.05 level of significance.

# (d)
# PB.Ratio = 5.7606 + (0.04501)*ROE + (0.4903)*LocalYes

# (e)
# PB.Ratio = 5.6365 + (0.04671 - 0.008970 * LocalYes)*ROE + (0.3541)*LocalYes

# (f)
# Model in part(e) is the best as its adjusted R-squared is highest at 0.977

# (g)
# As P-P plot has little variation, assumption of normal distribution is valid.
# As there is constant variance and the data points are in linear relationship, Assumptions for
# linear relationship and homoscedasticity are valid.
# Based on graphical representation, the regression assumptions are valid. 

# (h)
# The price-to-book ratio is 6.900

# (i)
# 95% prediction interval estimate for PB.Ratio of an international company if it has ROE of 24.1% 
# under the model in part(e) is (5.766. 7.758)

# (j)
# The simple linear regression equation for predicting all local companies' PB.Ratio is 
# PB.Ratio = 5.9905 + 0.03774ROE



