#### Stat 239 homework 3: shrinkage methods and cross-validation

##### Ridge Regression

library(ISLR)
prostate = read.table("prostate.data", header = T)

x = model.matrix(lpsa ~., prostate)[, -1]
y = prostate$lpsa

library(glmnet)

predict.regsubsets =function (object ,newdata ,id ,...){ 
		  form=as.formula(object$call [[2]]) 
		  mat=model.matrix(form,newdata) 
		  coefi=coef(object ,id=id) 
		  xvars=names(coefi) 
		  mat[,xvars]%*%coefi 
		} 
# lambda ranges from 10^-2 to 10^10
lam = 10^seq(10, -2, length = 100)
ridge = glmnet(x, y, alpha = 0, lambda = lam)

# get the 90th value for lambda
ridge$lambda[90]

coef(ridge)[,90]

# predict the coefficients given a new value of lambda
predict(ridge, s=1, type="coefficients")[1:10]

# cross validation
k = 10
set.seed(42)