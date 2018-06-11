#### STAT 239 HW 5 ####

# stuff to try:
#   - logistic regression with shrinkage (glmnet)         DONE
#   - lda                                                 DONE
#   - Random forest (use OOB test error as well as CV)
#   - Boosted trees
#   - SVM with different kernels
#   - variable selection for all techniques

# use LOOCV whenever possible

X = read.table("train.data")
Y = read.table("classtrain.txt")
train = as.data.frame(c(X,Y))
Xn = read.table("test.data")
Yn = 0
names(Yn) <- "V1.1"
test = as.data.frame(c(Xn,Yn))


library(MASS)
library(randomForest)
library(glmnet)
library(gbm)
library(e1071)

#### LOGISTIC REGRESSION ####
lam = 10^seq(10, -2, length=100)

K = 10
folds = sample(1:K, nrow(X), replace = TRUE)

glm.errors.r = matrix(NA, K, length(lam))	# ridge
glm.errors.l = matrix(NA, K, length(lam))  # lasso
glm.errors.e = matrix(NA, K, length(lam))  # elastic net

glm.err.r = rep(NA, length(lam))
glm.err.l = rep(NA, length(lam))
glm.err.e = rep(NA, length(lam))


#### COMPUTE TEST ERROR FOR EVERY VALUE OF LAMBDA AT EVERY FOLD
Xglm = model.matrix(V1.1~., data = train)
Yglm = train$V1.1



for(k in 1:K)
{
  glm.model.r = glmnet(Xglm[folds!=k,], Yglm[folds!=k], alpha=0, lambda=lam, family = "binomial")
  glm.model.l = glmnet(Xglm[folds!=k,], Yglm[folds!=k], alpha=1, lambda=lam, family = "binomial")
  glm.model.e = glmnet(Xglm[folds!=k,], Yglm[folds!=k], alpha=0.5, lambda=lam, family = "binomial")
  
  for(l in 1:length(lam))
  {
    glm.pred.r = predict(glm.model.r, s=lam[l], newx=Xglm[folds==k,], type = "class")  # ridge
    glm.pred.l = predict(glm.model.l, s=lam[l], newx=Xglm[folds==k,], type = "class")  # lasso
    glm.pred.e = predict(glm.model.e, s=lam[l], newx=Xglm[folds==k,], type = "class")  # elastic net

    glm.errors.r[k,l] = sum(Yglm[folds==k] != t(glm.pred.r))/length(Yglm[folds==k]) # proportion of misclassifications
    glm.errors.l[k,l] = sum(Yglm[folds==k] != t(glm.pred.l))/length(Yglm[folds==k])
    glm.errors.e[k,l] = sum(Yglm[folds==k] != t(glm.pred.e))/length(Yglm[folds==k])
  }
}

glm.err.r = apply(glm.errors.r, 2, mean)
glm.err.l = apply(glm.errors.l, 2, mean)
glm.err.e = apply(glm.errors.e, 2, mean)

glm.min.r = c(min(glm.err.r), which.min(glm.err.r)) # store the min cv error and the lambda index that produced it
glm.min.l = c(min(glm.err.l), which.min(glm.err.l))
glm.min.e = c(min(glm.err.e), which.min(glm.err.e))

glm.min.r # <---- Best
glm.min.l
glm.min.e

glm.min = glm.min.r # approx. 0.1972558

glm.model.r = glmnet(Xglm, Yglm, alpha=0, lambda=lam, family = "binomial")
glm.pred.r = predict(glm.model.r, s=lam[l], newx=as.matrix(Xn), type = "class")  # ridge



#### LDA ####
# cross validation
K = 10
folds = sample(1:K, nrow(X), replace = TRUE)
cv.err = rep(NA, K)

for(k in 1:K)
{
  lda.mod = lda(V1.1~., data = train[folds!=k,])
  lda.pred = predict(lda.mod, newdata = train[folds==k,1:31])
  cv.err[k] = sum(lda.pred$class != train[folds==k,32])/length(train[folds==k,32])
}
lda.min = mean(cv.err) # approx. 0.2987879

lda.mod = lda(V1.1~., data = train)
lda.pred = predict(lda.mod, newdata = Xn)

lda.pred$class ###########################


#### RANDOM FOREST ####
set.seed(3)
K = nrow(X)
folds = 1:K
rf.errors = rep(NA, K)
# try = tuneRF(x = train[,1:31], y = train[,32], mtryStart = 1, ntreeTry = 1000)

for(k in 1:K)
{
  rf.model = randomForest(as.factor(V1.1)~., data = train[folds!=k,], importance = TRUE, mtry = 1)
  rf.pred = predict(rf.model, newdata = train[folds==k,1:31], type="response")
  rf.errors[k] = sum(rf.pred != train[folds==k,32])/length(train[folds==k,32])
}

rf.min = mean(rf.errors)
rf.min # approx. 0.1653117

rf.model = randomForest(as.factor(V1.1)~., data = train, importance = TRUE, mtry = 1)
rf.pred = predict(rf.model, newdata = Xn, type="response")

rf.pred ###########################


#### BOOSTED TREES ####
set.seed(6969)

K = nrow(X)
folds = 1:K
gbm.errors = rep(NA, K)
for(k in 1:K)
{
  gbm.model = gbm(train[folds!=k,32] ~., data=train[folds!=k,], distribution = "bernoulli", n.trees = 2000, interaction.depth = 1, shrinkage = 0.01, cv.folds = 10)
  gbm.pred = predict(gbm.model, newdata = train[folds==k,])
  gbm.pred = round(exp(gbm.pred)/(1+exp(gbm.pred)))
  gbm.errors[k] = sum(gbm.pred != train[folds==k,32])/length(train[folds==k,32])
}

gbm.min = mean(gbm.errors)

gbm.model = gbm(train[1:42,32] ~., data=train[1:42,], distribution = "bernoulli", n.trees = 2000, interaction.depth = 1, shrinkage = 0.01, n.minobsinnode = 5)
gbm.pred = predict(gbm.model, newdata = train[43:82,], n.trees = 2000, type="response")
gbm.pred = round(gbm.pred)
gbm.errors = sum(gbm.pred != train[43:82,32])/length(train[43:82,32])

# fuck it let's just use boosting

gbm.mod = gbm(V1.1 ~., data=train, distribution = "bernoulli", n.trees = 2000, interaction.depth = 1, shrinkage = 0.01, n.minobsinnode = 10)

gbm.predtest = predict(gbm.mod, newdata = Xn, n.trees = 2000, type="response")
length(gbm.predtest)

gbm.final = round(gbm.predtest)

gbm.final ###########################

# fuck boosting

#### SVM ####
# radial kernel
K = nrow(X)
folds = 1:K
cv.errs = rep(NA, K)


for(k in 1:K)
{
  svm.model = svm(V1.1 ~ ., data = train[folds!=k,], kernel = "radial", type = "C", cost = 10^10)
  svm.pred = predict(svm.model, newdata = train[folds==k,])
  cv.errs[k] = sum(svm.pred != train[folds==k,32])/length(train[folds==k,32])
}
# best C is 10^10

svm.min = c(mean(cv.errs), which.min(cv.errs))
svm.min # approx. 0.195122

# polynomial kernel

K = nrow(X)
folds = 1:K
cv.errs = rep(NA, K)

for(k in 1:K)
{
  svm.model = svm(V1.1 ~ ., data = train[folds!=k,], kernel = "polynomial", degree = 1, type = "C", cost = 9^10)
  svm.pred = predict(svm.model, newdata = train[folds==k,])
  cv.errs[k] = sum(svm.pred != train[folds==k,32])/length(train[folds==k,32])
}
# best C is 10^10

svm.min2 = c(mean(cv.errs), which.min(cv.errs))
svm.min2 # approx. 0.2195122


svm.radial = svm(V1.1 ~ ., data = train, kernel = "radial", type = "C", cost = 10^10)
svm.pred.radial = predict(svm.radial, newdata = Xn)

svm.pred.radial ###########################

svm.poly = svm(V1.1 ~ ., data = train, kernel = "polynomial", degree = 1, type = "C", cost = 10^10)
svm.pred.poly = predict(svm.poly, newdata = Xn)

svm.pred.poly ###########################


#### K-MEANS ####
km.model = kmeans(Xn, 2, nstart = 50)
km.cluster = km.model$cluster - 1
km.cluster   ###########################

svmp = 1 - 0.2195122
svmr = 1 - 0.195122
gbm1 = 1 - 0.15
rf1 = 1 - 0.1653117
lda = 1 - 0.2987879
kmc = 1 - 0.15

weights = c(svmp, svmr, gbm1, rf1, lda, kmc)
total = sum(weights)

SVMPOLY1 = (as.double(svm.pred.poly) - 1) * svmp / total
SVMRADIAL = (as.double(svm.pred.radial) - 1) * svmr / total
GBM1 = as.double(gbm.final) * gbm1 / total
RF1 = (as.double(rf.pred) - 1) * rf1 / total
LDA = (as.double(lda.pred$class) - 1) * lda / total
KMC = km.cluster * kmc / total

compare = rbind(SVMPOLY1, SVMRADIAL, GBM1, RF1, LDA, KMC)

vote = round(apply(compare, 2, sum))

