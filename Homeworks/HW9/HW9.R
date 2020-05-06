RNGkind(sample.kind = "Rounding") # Change the random number generator (RNG) to the old version)
library(ISLR)
attach(College)
set.seed(169)

# sum(is.na(College))  # to check whether NA exists
# dim(College)         # to check the sample size and number of variable
train <- sample(1:nrow(College), 500)
test <- -train
College.train <- College[train,]
College.test <- College[test,]

# part(a): multiple linear regression
lm.fit <- lm(Apps~., data = College.train)
lm.pred <- predict(lm.fit, College.test)
mean((College.test$Apps - lm.pred)^2)   # test error is 932626.7
lm.fit$coefficients   # Coefficient estimates of the training model

# part(b): ridge regression model
library(glmnet)
train.x <- model.matrix(Apps~., data = College.train)[,-1]   # removes Y variable
train.y <- College.train$Apps
test.x <- model.matrix(Apps~., data = College.test)[,-1]
test.y <- College.test$Apps

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- cv.glmnet(train.x, train.y, alpha=0, lambda = grid)
lambda.rr <- ridge.mod$lambda.min     # or  lambda.rr <-min(ridge.mod$lambda)
lambda.rr    
ridge.pred <- predict(ridge.mod, newx = test.x, s=lambda.rr)
mean((ridge.pred-test.y)^2)       # Test error = 932532.7

x <- model.matrix(Apps~., data = College)[,-1]
y <- College$Apps
out.rr <- glmnet(x,y, alpha = 0, lambda = grid)   # use all the data to refit the model
rr.coef <- predict(out.rr, type="coefficients", s=lambda.rr)[1:18,]
rr.coef


# part(c): LASSO model
lasso.mod <- cv.glmnet(train.x, train.y, alpha=1, lambda=grid)
lambda.lasso <-lasso.mod$lambda.min
lambda.lasso
lasso.pred <- predict(lasso.mod, newx = test.x, s=lambda.lasso)
mean((test.y - lasso.pred)^2)     # Test Error = 852921.9

out.lasso <- glmnet(x,y, alpha = 1, lambda = grid)
lasso.coef <- predict(out.lasso, type = "coefficients", s=lambda.lasso)[1:18,]
lasso.coef[lasso.coef != 0]


## ANSWERs of Q1
# (a) Test Error = 932626.7
# (b) Test Error = 932532.7
# (c) Test Error = 852921.9