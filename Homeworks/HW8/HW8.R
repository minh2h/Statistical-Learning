RNGkind(sample.kind = "Rounding") # Change the random number generator (RNG) to the old version)
library(ISLR)
View(Credit)
attach(Credit)
names(Credit)
dim(Credit)
library(leaps)  # new library to run the Subset Selection

regfit1.all <- regsubsets(Balance~.-ID, Credit, nvmax = 10)
reg1.summary <- summary(regfit1.all)
coef(regfit1.all, which.min(reg1.summary$bic))
plot(reg1.summary$bic, main="BIC plot", 
     xlab="Number of variables", ylab="BIC", type="b")

regfit2.all <- regsubsets(Balance~.-ID, Credit, nvmax = 10, method = "forward")
reg2.summary <- summary(regfit2.all)
coef(regfit2.all, which.min(reg2.summary$cp))
plot(reg2.summary$cp, main="Forward Selection with Cp", 
     xlab="Number of variables", ylab="Cp", type="b")

regfit3.all <- regsubsets(Balance~.-ID, Credit, nvmax = 10, method = "backward")
reg3.summary <- summary(regfit3.all)
coef(regfit3.all, which.max(reg3.summary$adjr2))
plot(reg3.summary$adjr2, main="Backward Selecion with Adjusted r^2", 
     xlab="Number of variables", ylab="Adjusted r^2", type="b")

set.seed(121)
# use validation set approach to select the model
train <- sample(c(TRUE, FALSE), nrow(Credit), rep=TRUE)  # get the random training set
test <- (!train)                # set the rest as test set

regfit4 <- regsubsets(Balance~.-ID, Credit[train,], nvmax = 10) #run the best selection on training set
test.mat <- model.matrix(Balance~.-ID, Credit[test,])  # make the model matrix from the test data
val.error <- rep(NA, 10)

for (i in 1:10) {
  coefi <- coef(regfit4, id=i) # extract the coefficients
  pred <- test.mat[, names(coefi)]%*%coefi # multiply the coefficients to form the prediction
  val.error[i] <- mean((Credit$Balance[test]-pred)^2) # compute the test MSE
}
val.error
plot(val.error, main = "Best subset Selection under Validation Approach",
     xlab = "Number of variables", ylab = "validation mean square error", type = "b")
d <- which.min(val.errors)
d
# use all data points to get the estimates
coef(regfit1.all, d)

# ANSWERS
# (a) Balance = -499.727 - 7.839*(Income) + 0.267*(Limit) + 23.17*(Cards) + 429.606*(StudentYes)
# (b) Balance = -493.734 - 7.795*(Income) + 0.194*(Limit) + 1.091*(Rating) + 18.212*(Cards) -
#                0.624*(Age) + 425.61*(StudentYes)
# (c) Balance = -488.616 - 7.804*(Income) + 0.194*(Limit) + 1.094*(Rating) + 18.109*(Cards) - 
#                0.621*(Age) - 10.453*(GenderFemale) + 426.581*(StudentYes)
# (d) Balance = -499.73 - 7.8392*(Income) + 0.26664*(Limit) + 23.175*(Cards) + 429.61*(StudentYes)
#      under Best Subset Selection with validation approach

