library(ISLR)
attach(OJ)
str(OJ)
RNGkind(sample.kind = "Rounding") # Change the random number generator (RNG) to the old version)
set.seed(103)

# (a)
train <- sample(dim(OJ)[1], 800)
OJ.train <- OJ[train,]
OJ.test <- OJ[-train,]

# (b)
library(tree)
oj.tree <- tree(Purchase~., data = OJ.train)
summary(oj.tree)

# (c)
plot(oj.tree)
title("Classification tree analysis of OJ data ")
text(oj.tree, pretty = 0)

# (d)
oj.pred <- predict(oj.tree, OJ.test, type = "class")
table1 <- table(OJ.test$Purchase, oj.pred)
table1
test.error <- (table1[1,2] + table1[2,1]) / sum(table1)
test.error
# confusionMatrix(data = oj.pred, reference = OJ.test$Purchase)  

# (e)
cv.oj <- cv.tree(oj.tree, FUN = prune.misclass)
# plot(cv.oj$size, cv.oj$dev, type="b", main="Pruning classification tree: size vs deviance",
#    xlab="number of trees", ylab="error")
nn <- cv.oj$size[which.min(cv.oj$dev)]
nn

# (f)
oj.pruned <- prune.misclass(oj.tree, best = nn)
summary(oj.pruned)

# (g)
pred.pruned <- predict(oj.pruned, OJ.test, type = "class")
table.pruned <- table(OJ.test$Purchase, pred.pruned)
table.pruned
error.pruned <- (table.pruned[1,2] + table.pruned[2,1]) / sum(table.pruned)
error.pruned

# (h) build the final tree with all data
ojall.tree <- tree(Purchase~., data = OJ)
ojall.pruned <- prune.misclass(ojall.tree, best = nn)
plot(ojall.pruned)
title(main = "Pruned tree of all OJ data set")
text(ojall.pruned, pretty = 0)



# ANSWERS
# (a) In OJ.train and OJ.test
# (b) The training error rate is 0.1662 and number of terminal nodes is 8
# (d) The test error rate is 0.2185
# (e) The optimal size is 4
# (f) The training error rate is 0.1662
# (g) The test error rate is 0.2185
# (i) 