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
library(e1071)


set.seed(42)
train = read.table("train.txt", header = T)
test = read.table("test.txt", header = T)
x.train = model.matrix(y ~., train)
train$y = train$y - 1
y.train = train$y

x.test = model.matrix(y ~., test)
test$y = test$y - 1
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
#### LASSO ####

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

plot(shrink, mean.cv.errors, xlim=c(0,1), ylim=c(0.05, 0.3), ylab="lasso CV error (assuming orthogonal X)", xlab="shrinkage factor", main="Lasso CV error VS shrinkage", cex=2, type="l")
errbar(shrink, mean.cv.errors, yplus=mean.cv.errors+stderr, yminus=mean.cv.errors-stderr, add=TRUE)

lasso.best = min(mean.cv.errors) ##  0.08797778
bestS = shrink[which.min(mean.cv.errors)]
abline(v = bestS, lty=2)

# Lasso test error
lasso = glmnet(x.train, y.train, alpha = 1, lambda = lam)
lasso.pred = predict.glmnet(lasso, s=lam[which.min(mean.cv.errors)], newx = x.test)
lasso.pred = round(lasso.pred, digits = 0)
lo.testerr = sum(lasso.pred != y.test)  # number of misclassifications for the best Lasso model on the test set

#### RANDOM FOREST ####

set.seed(3)
rf.model = randomForest(as.factor(y) ~., data=train, importance = T)
rf.pred = predict(rf.model, newdata = test, type="response")
t = table(rf.pred, y.test) # 0.953125 -- same as lasso. let's see if we can make it better
error = (t[1,1] + t[2,2])/(sum(t))
error # 0.9453125

try = tuneRF(x = x.train, y = as.factor(y.train), mtryStart = 1, ntreeTry = 1000) # 1 is best number of variables to select

# Random forest test error
rf.model2 = randomForest(as.factor(y) ~., data=train, importance = T, mtry = 1)
rf.pred2 = predict(rf.model2, newdata = test, type="response")
rf.testerr = sum(rf.pred2 != y.test) # number of misclassifications for random forest with m = 1 on the test set

varImpPlot(rf.model2)
imp <- importance(rf.model2)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
partialPlot(rf.model2, pred.data=train, x.var=impvar[1])


#### BOOSTED TREES ####

set.seed(9)
boost.model4 = gbm(y ~., data=train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01, cv.folds = 10)
summary(boost.model4)
cvp4 = gbm.perf(boost.model4, method="cv") # order of interaction  4

boost.model3 = gbm(y ~., data=train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 3, shrinkage = 0.01, cv.folds = 10)
cvp3 = gbm.perf(boost.model3, method="cv") # order of interaction  3

boost.model2 = gbm(y ~., data=train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 2, shrinkage = 0.01, cv.folds = 10)
cvp2 = gbm.perf(boost.model2, method="cv") # order of interaction  2

boost.model1 = gbm(y ~., data=train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 1, shrinkage = 0.01, cv.folds = 10)
cvp1 = gbm.perf(boost.model1, method="cv") # order of interaction  1

plot(boost.model4$cv.error, col="green", type="l")
lines(boost.model3$cv.error, col="blue", type="l")
lines(boost.model2$cv.error, col="red", type="l")
lines(boost.model1$cv.error, col="black", type="l")

min(boost.model4$cv.error)
min(boost.model3$cv.error)
min(boost.model2$cv.error)
min(boost.model1$cv.error)  # <--- smallest

which.min(boost.model1$cv.error)  # = cvp1 = 1148

# stumps seem to be the best base learners.

set.seed(9)
boost.pred1 = predict(boost.model1, newdata=test, n.trees = cvp1, type="response")
boost.pred1 = round(boost.pred1, 0)
bt.testerr = sum(boost.pred1 != y.test) # number of misclassifications for Boosted Trees with M = 1 on the test set


#### SVM ####
## Polynomial kernel

## determining the best value for the Cost C by CV
K = 10
C = 10^seq(10, -2, length=100)
folds = sample(1:K, nrow(x.train), replace = TRUE)
cv.errors = matrix(NA, K, length(C))
cv.errs = rep(C)

for(k in 1:K)
{
  for(c in 1:length(C))
  {
    svm.model = svm(y ~ ., data = train[folds!=k,], kernel = "polynomial", degree = 1, type = "C", cost = c)
    svm.pred = predict(svm.model, newdata = train[folds==k,])
    cv.errors[k, c] = sum(svm.pred != y.train[folds==k])
  }
}
cv.errs = apply(cv.errors, 2, mean)
bestC = C[which.min(cv.errs)]
min(cv.errs)
bestC

#### CV error VS cost for polynomial SVM of degree 1
plot(log(C), cv.errs, type="l", main = "CV error VS log(cost) for polynomial SVM of degree 1")
abline(v = log(bestC), lty=2)

## determining the best value for the polynomial degree by CV
K = 10
P = 8
folds = sample(1:K, nrow(x.train), replace = TRUE)
cv.errors2 = matrix(NA, K, P)
cv.errs2 = rep(P)

for(k in 1:K)
{
  for(p in 1:P)
  {
    svm.model = svm(y ~ ., data = train[folds!=k,], kernel = "polynomial", degree = p, type = "C", cost = bestC)
    svm.pred = predict(svm.model, newdata = train[folds==k,])
    cv.errors2[k, p] = sum(svm.pred != y.train[folds==k])
  }
}
cv.errs2 = apply(cv.errors2, 2, mean)
min(cv.errs2) # = 1.8 ---> polynomial degree 3


svm.model3 = svm(y ~ ., data = train, kernel = "polynomial", degree = 3, type = "C", cost = bestC)
svm.pred3 = predict(svm.model3, newdata=test)
svm.testerr3 = sum(svm.pred3 != test$y) # number of misclassifications by svm polynomial kernel degree 3 on the test set


## Radial Kernel

K = 10
C = 10^seq(10, -2, length=100)
folds = sample(1:K, nrow(x.train), replace = TRUE)
cv.errors2 = matrix(NA, K, length(C))
cv.errs2 = rep(C)

for(k in 1:K)
{
  for(c in 1:length(C))
  {
    svm.model = svm(y ~ ., data = train[folds!=k,], kernel = "radial", type = "C", cost = c)
    svm.pred = predict(svm.model, newdata = train[folds==k,])
    cv.errors2[k, c] = sum(svm.pred != y.train[folds==k])
  }
}

cv.errs2 = apply(cv.errors2, 2, mean)
bestC2 = C[which.min(cv.errs2)]
min(cv.errs2)

svm.model4 = svm(y ~ ., data = train, kernel = "radial", type = "C", cost = bestC2)
svm.pred4 = predict(svm.model4, newdata = test)
svm.testerr4 = sum(svm.pred4 != y.test) # number of misclassifications by svm radial kernel on the test set

#### CV ERROR PLOTS FOR RF AND BT ####

K = 10  # CV folds
R = 20  # number of trees to try RF and Boosting on
folds = sample(1:K, nrow(x.train), replace = T)
rf.error = matrix(NA, K, R)
bt.error = matrix(NA, K, R)
rf.err = rep(NA, R)
bt.err = rep(NA, R)

for(r in 1:R)
{
  for(k in 1:K)
  {
    rf.model2 = randomForest(as.factor(y) ~., data=train[folds!=k,], importance = TRUE, mtry = 1, ntree = r)
    rf.pred2 = predict(rf.model2, newdata = train[folds==k,], type="response")
    rf.error[k, r] = sum(rf.pred2 != y.train[folds==k])
    
    boost.model1 = gbm(y ~., data=train[folds!=k,], distribution = "bernoulli", n.trees = r, interaction.depth = 1, shrinkage = 0.01)
    boost.pred1 = predict(boost.model1, newdata=train[folds==k,], n.trees = r, type="response")
    boost.pred1 = round(boost.pred1, 0)
    bt.error[k, r] = sum(boost.pred1 != y.train[folds==k])
  }
}

bt.err = apply(bt.error, 2, mean)
rf.err = apply(rf.error, 2, mean)

# Boosting and Random Forest
plot(1:R, rf.err, type = "l", ylab="average number of misclassifications", xlab = "number of trees", main = "CV error comparison of RF and GBM")
lines(1:R, bt.err, type = "l", col = "blue")
legend("topright", legend = c("Random Forest, mtry = 1", "GBM, M = 1 (stumps)"), col = c("black", "blue"), lty=1)


# final comparison of test errors
barplot(height = c(lo.testerr, 
                   rf.testerr, 
                   bt.testerr, 
                   svm.testerr3, 
                   svm.testerr4), 
        space = 1, 
        names.arg = c("Lasso", 
                      "RF, m=1", 
                      "GBM, M=1",
                      "SVM Poly 3",
                      "SVM Radial"), 
        main = "Comparison of test errors", 
        ylab = "number of misclassifications", 
        col = c("darkolivegreen1", 
                "darkolivegreen2", 
                "darkolivegreen3", 
                "darkolivegreen4",
                "darkolivegreen"))


### questions:
# - if in random forest the best number of predictor to sample from at each bagging iteration is low, does this mean the 
#   data are very correlated? how can one get insight into the properties of the data by examining the models that fit it best?
