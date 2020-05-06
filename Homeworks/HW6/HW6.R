# Logistic regression model
InsuranceFraud <- read.csv("/Volumes/STORE N GO/DSA211/Homework/HW6/InsuranceFraud.csv")
attach(InsuranceFraud)
summary(InsuranceFraud)

# (a)
glm.h1 <- glm(Fraud~ NewB+Claims, data = InsuranceFraud, family = binomial)
summary(glm.h1)

# (c)
glm.h2 <- glm(Fraud~ Claims, data = InsuranceFraud, family = binomial)
summary(glm.h2)

# (d)
glm.h3 <- glm(Fraud~ NewB, data = InsuranceFraud, family = binomial)
summary(glm.h3)

# (e)
glm.h4 <- glm(Fraud~ NewB*Claims, data = InsuranceFraud, family = binomial)
summary(glm.h4)

# (f)
pvalue1 <- 1 - pchisq(38.626, 95)
pvalue2 <- 1 - pchisq(52.085, 96)
pvalue3 <- 1 - pchisq(119.47, 96)
pvalue4 <- 1 - pchisq(37.319, 94)
pvalue1
pvalue2
pvalue3
pvalue4

# (g)
glm.h1$aic
glm.h2$aic
glm.h3$aic
glm.h4$aic

# (h)
predict(glm.h1, data.frame(NewB="No", Claims=1.5), type = "response")
# (i)
predict(glm.h1, data.frame(NewB="Yes", Claims=0.5), type = "response")

# (j)
# set the threshold value to 0.5
glm.prob <- predict(glm.h1, type = "response")
glm.pred <- rep("Predicted non-fraudulent", 98)
glm.pred[glm.prob > 0.5] <- "Predicted fradulent"
table1 <- table(glm.pred, InsuranceFraud$Fraud)
table1
overall_errorRate <- (table1[1,1]+table1[2,2])/sum(table1)
overall_errorRate

  # ANSWERS OF Q1

# (a) ln(estimated odds) = -7.713 + 3.812*(NewB) + 6.303*(Claims)

# (b) Holding constant the effects of whether the policy is new, for each 
#     increase of the number of claims submitted per year by the policy
#     holder, ln(odds) increases by 6.303. 
#     Holding constant the number of claims submitted  per year by the
#     policy holder, ln(odds) is estimated to be 3.812 higher when the 
#     policy is new as compared to when it is not.

# (c) ln(estimated odds) = -5.0293 + 4.8041*(Claims)

# (d) ln(estimated odds) = -0.5423 + 1.9286*(NewB)

# (e) ln(estimated odds) = -6.79302 + -0.09905*(NewB) + 5.58533*(Claims) +
#                          6.10630*(NewB*Claims)

# (f) Since p-values for models in parts (a),(c),(d) and (e) are
#     1, 1, 0.0526 and 1 respectively, we cannot reject null hypothesis:
#     The logistic regressuin models is a good fitting model.

# (g) Model (a) is the best as it has the smallest AIC: 44.626 while
#     AIC for models (c), (d) and (e) are 56.085, 123.47 and 45.319 respectively

# (h) estimated probability = 0.8509

# (i) estimated probability = 0.3210

# (j) 
#  glm.pred                     No   Yes
#    Predicted fradulent         3    46
#    Predicted non-fraudulent   46     3
# Its overall rate = 0.0612






