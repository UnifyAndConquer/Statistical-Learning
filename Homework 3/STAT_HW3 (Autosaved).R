#### Stat 239 homework 3: shrinkage methods and cross-validation

#### Ridge Regression

library(ISLR)
prostate = read.table("prostate.data", header = T)

set.seed(42)
x = model.matrix(lpsa ~., prostate)[, -1]
y = prostate$lpsa

x.train = x[prostate$train==T,]
y.train = y[prostate$train==T]

x.test = x[prostate$train==F,]
y.test = y[prostate$train==F]


library(glmnet)
library(rms)

predict=function (object ,newdata ,id ,...)
{ 
	form=as.formula(object$call [[2]]) 
	mat=model.matrix(form,newdata) 
	coefi=coef(object ,id=id) 
	xvars=names(coefi) 
	mat[,xvars]%*%coefi 
} 


# Start by computing the cv test error of the ridge model. This is done by calling the predict function on glmnet for each fold, i.e. for each of the ten samples chosen randomly for cross validation (use newx = fold elements).

# evaluates the coefficients for every value of lambda in lam
ridge = glmnet(x.train, y.train, alpha=0, lambda=lam)

# shows the coefficients for the 50th lambda value 
coef(ridge)[,50] 

# predicts the coefficients for a new value of lambda
pred.coef = predict(ridge, s=50, type="coefficients")[1:10]

pred.y = predict(ridge, s=1, newx = x.test)
pred.y

k = 10
folds = sample(1:k, nrow(x.train), replace = TRUE)
cv.errors = matrix(NA, k, 100, dimnames = list(NULL, paste(1:9)))
lam = 10^seq(10, -2, length=100)


for(i in 1:k)
{
	ridge.coefs = glmnet(x.train[folds!=i,], y.train[folds!=i], alpha=0, lambda=lam)
	for(l in lam)
	{
		pred = predict(ridge.coefs, s=l, newx=x.train[folds=i])
	}
}




# 10-fold cross validation
cv.ridge = cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.ridge)

#### Lasso
cv.lasso = cv.glmnet(x.train, y.train, alpha = 1)
plot(cv.lasso)



#### everything should be done with resp to training set. test set at the end, to get test error of final model

#### svd(X) to find d values, calculate df(F) manually

#### for ridge, compute cv manually and find df. Plot the test error wrsp to df for the regular matrix, and assuming the atrix is orthogonal.

#### for lasso, clso compute cv manually, and find out how to get the shrinkage factor s from t. understand what t is.

#### do the uncertainty of the test error using bootstrap.

############## TESTING AREA ###############

x[prostate$train==T,]
