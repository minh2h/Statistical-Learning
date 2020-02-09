GPAGMAT <- read.csv("/Volumes/STORE N GO/DSA211/Homework/HW4/GPAGMAT.csv")
summary(GPAGMAT)
gmat<- GPAGMAT$GMAT
gpa <- GPAGMAT$GPA
reg.fit <- lm(gpa~gmat)
summary(reg.fit)

# scatter plot with estimated linear regression equation
plot(gmat, gpa, main="Linear relationship between 
      GPA and GMAT score", xlab = "GMAT", ylab = "GPA")

abline(reg.fit, lwd=3, col = "orange")

summary(reg.fit)

# checking assumptions
plot(gmat, residuals(reg.fit), main = 'Relationship between GMAT scores and residuals',
     xlab = 'GMAT scores', ylab = 'Residuals')

library(fitdistrplus)
fnorm <- fitdist(residuals(reg.fit), "norm")
summary(fnorm)
plot(fnorm)

# (e)
confint(reg.fit, level = 0.9)
# (f)
predict(reg.fit, data.frame(gmat=c(600,630,660,690)), interval = 'confidence', level = 0.95)
# (g)
predict(reg.fit, data.frame(gmat=c(600,630,660,690)), interval = 'prediction', level = 0.99)

# Answers
# (a) Refer to R-output

# (b) Y-intercept = -0.8887890 and slope = 0.0067005 and adjusted r^2 = 0.7614 

# (c) The scatter plot shows the linear relationship between GPA and GMAT scores. 
#     No pattern is found in the residuals vs GMAT plot.
#     P-P plot and Q-Q plot shows no deivation from the normal distribution assumption.

# (d) As p-value < 2.2e-16 < 0.05, we reject null hypothesis: β1=0, thus the data provide sufficient
#     evidence that there is a linear relationship between GMAT score and GPA.
#     The F-statistic is 253.2 and p-value is less than 2.2e-16

# (e) 90% confidence interval estimate of the population slope is (0.006,0.0074)

# (f) 95% confidence interval estimates of the mean GPA of students with GMAT score of 600 is
#     (3.088, 3.175)
#     95% confidence interval estimates of the mean GPA of students with GMAT score of 630 is
#     (3.298, 3.367)
#     95% confidence interval estimates of the mean GPA of students with GMAT score of 660 is
#     (3.492, 3.576)
#     95% confidence interval estimates of the mean GPA of students with GMAT score of 690 is
#     (3.674, 3.795)

# (e) 99% prediction interval estimates of the GPA for an individual with GMAT score of 600 is
#     (2.720, 3,543)
#     99% prediction interval estimates of the GPA for an individual with GMAT score of 630 is
#     (2.923, 3.742)
#     99% prediction interval estimates of the GPA for an individual with GMAT score of 660 is
#     (3.122, 3.945)
#     99% prediction interval estimates of the GPA for an individual with GMAT score of 690 is
#     (3.320, 4.150)



