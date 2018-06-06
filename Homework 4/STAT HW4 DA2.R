#### STAT HW4 DA2 ####
#### Comparison of the predictive performance of RF, SVM, Boosted trees and Lasso

                  #### INITIALIZATION ####
library(ISLR)
library(glmnet)
library(rms)
library(pls)
library(tree)
library(randomForest)
library(gbm)


set.seed(42)
train = read.table("train.txt", header = T)
test = read.table("test.txt", header = T)
x.train = model.matrix(y ~., train)
y.train = train$y

x.test = model.matrix(y ~., test)
y.test = test$y

lam = 10^seq(10, -2, length=100)


                  #### FUNCTIONS ####

# predict <- function (object, newdata, id, ...)
# { 
#   form=as.formula(object$call [[2]]) 
#   mat=model.matrix(form,newdata) 
#   coefi=coef(object ,id=id) 
#   xvars=names(coefi)
#   mat[,xvars]%*%coefi 
# } 

bestModel <- function(x, y, stderr)
{
  min.index = which.min(y)
  min.stddev = y[min.index] + stderr[min.index]
  abline(h = min.stddev, lty=2)
  best.index = which.min(y[y>= min.stddev]) + 1
  best.model = x[best.index]
  return(c(best.model, best.index))
}


                  #### REFERENCE ####

#### EVALUATES THE COEFFICIENTS FOR EVERY VALUE OF LAMBDA IN LAM
lasso = glmnet(x.train, y.train, alpha=1, lambda=lam)

#### SHOWS THE COEFFICIENTS FOR THE 50TH LAMBDA VALUE 
coef(lasso)[,50]

#### PREDICTS THE COEFFICIENTS FOR A NEW VALUE OF LAMBDA
pred = predict.glmnet(lasso, s=50, type="coefficients")[1:21]

#### PREDICTS THE Y'S FOR NEW VALUES OF X GIVEN LAMBDA
pred.y = predict.glmnet(ridge, s=1, newx = x.test)


                  #### MODEL COMPARISON ####

k = 10
folds = sample(1:k, nrow(x.train), replace = T)
cv.errors = matrix(NA, k, 100)
shrink = rep(0, length(lam))   # shrinkage factor for lasso

#### COMPUTE TEST ERROR FOR EVERY LAMDBA AT EVERY FOLD ####
for(i in 1:k)
{
  lasso.model = glmnet(x.train[folds!=i,], y.train[folds!=i], alpha=1, lambda=lam)
  
  for(l in 1:length(lam))
  {
    lasso.pred = predict.glmnet(lasso.model, s=lam[l], newx=x.train[folds==i,])
    cv.errors[i, l] = mean((y.train[folds==i] - t(lasso.pred))^2)

    # compute shrinkage factor
    shrink[l] = sum(lasso.model$beta[,l] / sum(lasso.model$beta[,100]))
  }
}

std <- function(x) sqrt(var(x)/length(x))
stderr = apply(cv.errors, 2, std)
mean.cv.errors = apply(cv.errors, 2, mean)

plot(shrink, mean.cv.errors, xlim=c(0,1), ylim=c(0.05, 0.3), ylab="lasso test error (assuming orthogonal X)", xlab="shrinkage factor", main="Lasso", cex=2, type="l")
errbar(shrink, mean.cv.errors, yplus=mean.cv.errors+stderr, yminus=mean.cv.errors-stderr, add=TRUE)

lasso.best = min(mean.cv.errors) ##  0.08517907
bestS = shrink[which.min(mean.cv.errors)]
abline(v = bestS, lty=2)

#### COMPUTING TEST ERROR ####

lasso = glmnet(x.train, y.train, alpha = 1, lambda = lam)
lasso.pred = predict.glmnet(lasso, s=lam[which.min(mean.cv.errors)], newx = x.test)
lasso.pred = round(lasso.pred, digits = 0)
table(lasso.pred, y.test)
# (64+58)/(64+58+4+2) = 0.953125

#### TREE ####

par(mfrow=c(1, 1))

tree.train = tree(as.factor(y) ~., train)
plot(tree.train)
text(tree.train, pretty=0)
summary(tree.train)
tree.train

cv = cv.tree(tree.train)

prune = prune.misclass(tree.train, best = 3)
plot(prune)
text(prune, pretty=0)

#### RANDOM FOREST ####

set.seed(3)
rf.model = randomForest(as.factor(y) ~., data=train, importance = T)
rf.pred = predict(rf.model, newdata = test, type="response")
t = table(rf.pred, y.test) # 0.953125 -- same as lasso. let's see if we can make it better
error = (t[1,1] + t[2,2])/(sum(t))
error # 0.9453125

try = tuneRF(x = x.train, y = as.factor(y.train), mtryStart = 1, ntreeTry = 1000) # 1 is best number of variables to select

rf.model2 = randomForest(as.factor(y) ~., data=train, importance = T, mtry = 1)
rf.pred2 = predict(rf.model2, newdata = test, type="response")
t2 = table(rf.pred2, y.test)
error2 = (t2[1,1] + t2[2,2])/(sum(t2))
error2 # 0.9765625

varImpPlot(rf.model2)
imp <- importance(rf.model2)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
partialPlot(rf.model2, pred.data=train, x.var=impvar[1])


#### BOOSTED TREES ####

train$y = train$y - 1

set.seed(9)
boost.model4 = gbm(y ~., data=train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01, cv.folds = 10)
summary(boost.model4)
cvp4 = gbm.perf(boost.model4, method="cv") # optimal M for order of interaction  4

boost.model3 = gbm(y ~., data=train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 3, shrinkage = 0.01, cv.folds = 10)
cvp3 = gbm.perf(boost.model3, method="cv") # optimal M for order of interaction  3

boost.model2 = gbm(y ~., data=train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 2, shrinkage = 0.01, cv.folds = 10)
cvp2 = gbm.perf(boost.model2, method="cv") # optimal M for order of interaction  2

boost.model1 = gbm(y ~., data=train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 1, shrinkage = 0.01, cv.folds = 10)
cvp1 = gbm.perf(boost.model1, method="cv") # optimal M for order of interaction  1

plot(boost.model4$cv.error, col="green", type="l")
lines(boost.model3$cv.error, col="blue", type="l")
lines(boost.model2$cv.error, col="red", type="l")
lines(boost.model1$cv.error, col="black", type="l")

# stumps seem to be the best base learners.

test$y = test$y - 1

set.seed(9)
boost.pred1 = predict(boost.model1, newdata=test, n.trees = cvp1, type="response")
boost.pred1 = round(boost.pred1, 0)
t3.1 = table(boost.pred1, test$y)
error3.1 = (t3.1[1,1] + t3.1[2,2])/(sum(t3.1))
error3.1  # 0.9296875

set.seed(10)
boost.pred2 = predict(boost.model2, newdata=test, n.trees = cvp1, type="response")
boost.pred2 = round(boost.pred2, 0)
t3.2 = table(boost.pred2, test$y)
error3.2 = (t3.2[1,1] + t3.2[2,2])/(sum(t3.2))
error3.2  # 0.953125

set.seed(11)
boost.pred3 = predict(boost.model3, newdata=test, n.trees = cvp1, type="response")
boost.pred3 = round(boost.pred3, 0)
t3.3 = table(boost.pred, test$y)
error3.3 = (t3.3[1,1] + t3.3[2,2])/(sum(t3.3))
error3.3  # 0.9296875

set.seed(12)
boost.pred4 = predict(boost.model4, newdata=test, n.trees = cvp1, type="response")
boost.pred4= round(boost.pred4, 0)
t3.4 = table(boost.pred4, test$y)
error3.4 = (t3.4[1,1] + t3.4[2,2])/(sum(t3.4))
error3.4  # 0.9453125


#### SVM ####



