# Question 1

library(boot)
library(ISLR)
library(MASS)
attach(Carseats)
RNGkind(sample.kind = "Rounding") # Change the random number generator (RNG) to the old version)

lm.fit1 <- lm(Sales~poly(Price, 1, raw = TRUE), data = Carseats)
lm.fit2 <- lm(Sales~poly(Price, 2, raw = TRUE), data = Carseats)
lm.fit3 <- lm(Sales~poly(Price, 3, raw = TRUE), data = Carseats)
lm.fit4 <- lm(Sales~poly(Price, 4, raw = TRUE), data = Carseats)
summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)
summary(lm.fit4)

set.seed(123)
cv.error1 <- rep(0,4)   # for LOOCV
cv.error2 <- rep(0,4)   # for 10-fold CV
glm.f1 <- glm(Sales~poly(Price,1,raw = TRUE), data = Carseats)
glm.f2 <- glm(Sales~poly(Price,2,raw = TRUE), data = Carseats)
glm.f3 <- glm(Sales~poly(Price,3,raw = TRUE), data = Carseats)
glm.f4 <- glm(Sales~poly(Price,4,raw = TRUE), data = Carseats)
cv.error1[1] <- cv.glm(Carseats, glm.f1)$delta[1]
cv.error1[2] <- cv.glm(Carseats, glm.f2)$delta[1]
cv.error1[3] <- cv.glm(Carseats, glm.f3)$delta[1]
cv.error1[4] <- cv.glm(Carseats, glm.f4)$delta[1]
cv.error1

cv.error2[1] <- cv.glm(Carseats, glm.f1, K=10)$delta[1]
cv.error2[2] <- cv.glm(Carseats, glm.f2, K=10)$delta[1]
cv.error2[3] <- cv.glm(Carseats, glm.f3, K=10)$delta[1]
cv.error2[4] <- cv.glm(Carseats, glm.f4, K=10)$delta[1]
cv.error2
summary(glm.f4)

# ANSWERS Of Q1
# (a) S = 13.64 - 0.053P   where S = Sales and P = Price
#     S = 14.27 - 0.06459 + 5.038e-05P^2
#     S = 16.34 - 0.1272P + 6.424e-04P^3 - 1.771e-06P^3
#     S = 2.553+0.522P - 0.0096P^2 + 6.497e-05P^3 - 1.529e-07P^4
# (b) Polynomial with order 4 is the best model among the four models 
#     since it has the highest adjusted R-squared of 0.2033
# (c) Cross-validation errors for polynomial regression models 1,2,3,and 4
#     are 6.444, 6.484, 6.624 and 6.387, respectively.
# (d) Polynomial with order 4 should be used as it has the smallest 
#     cross-validation error of 6.387 in LOOCV. 
#     S = 2.553 + 0.522P - 0.0096P^2 + 6.497e-05P^3 - 1.529e-07P^4
# (e) 10-fold cross-validation errors for polynomial regression models 1,2,3,and 4
#     are 6.436, 6.463, 6.693 and 6.387, respectively.
# (f) Polynomial with order 4 should be used as it has the smallest 
#     cross-validation error of 6.387 with k=10. 
#     S = 2.553 + 0.522P - 0.0096P^2 + 6.497e-05P^3 - 1.529e-07P^4




############################################################################
# Question 2 
library(MASS)
attach(Boston)
set.seed(456)
mdev <- Boston$medv
mean_medv <- mean(medv)    # estimate for popn mean
se.medv <- sd(medv)/sqrt(length(medv))     # standard error of popn mean
mean_medv
se.medv

# direct calculate the CI using formula
# ME_medv <- qt(0.975, df=length(medv)-1)*se.medv
# CI_medv <- c(mean_medv - ME_medv, mean_medv+ME_medv)

# calculate the CI using the R-function
t.test(medv)$conf

# q2d & q2e
boot.fn1 <- function(data, index){
    return(mean(data[index]))
}
bs_result1 <- boot(medv, boot.fn1, 10000)
bs_result1   
c(quantile(bs_result1$t, 0.025), quantile(bs_result1$t, 0.975))

# q2f, q2g & q2h
boot.fn2 <- function(data, index){
    return(quantile(data[index],0.75) - quantile(data[index],0.25))
}
bs_result2 <- boot(medv, boot.fn2, 10000)
bs_result2 
c(quantile(bs_result2$t, 0.025), quantile(bs_result2$t, 0.975))


# ANSWERS of Q2
# (a) u = 22.5328
# (b) sd(u) = 0.4089
# (c) 95% Confidence Interval for u is (21.7295, 23.3361)
# (d) u = 22.5328 - 0.0077 = 22.525 ;  sd(u) = 0.4017
# (e) 95% Confidence Interval for u is (21.7583, 23.3277)
# (f) θ = 7.975 + 0.4721 = 8.4471
# (g) sd(θ) = 0.8715
# (h) 95% Confidence Interval for θ is (7.175, 10.400)



