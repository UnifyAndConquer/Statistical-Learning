#### STAT 239 HOMEWORK 3: SHRINKAGE METHODS AND CROSS-VALIDATION


library(ISLR)
prostate = read.table("prostate.data", header = T)

set.seed(42)
x.train = model.matrix(lpsa ~., prostate[prostate$train==TRUE,])[, -1]
y.train = prostate$lpsa[prostate$train==TRUE]

x.test = model.matrix(lpsa ~., prostate[prostate$train==FALSE,])[, -1]
y.test = prostate$lpsa[prostate$train==FALSE]


library(glmnet)
library(rms)
library(pls)

						#### FUNCTIONS ####

predict <- function (object, newdata, id, ...)
{ 
	form=as.formula(object$call [[2]]) 
	mat=model.matrix(form,newdata) 
	coefi=coef(object ,id=id) 
	xvars=names(coefi) 
	mat[,xvars]%*%coefi 
} 

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
ridge = glmnet(x.train, y.train, alpha=0, lambda=lam)

#### SHOWS THE COEFFICIENTS FOR THE 50TH LAMBDA VALUE 
coef(ridge)[,50] 

#### PREDICTS THE COEFFICIENTS FOR A NEW VALUE OF LAMBDA
pred = predict.glmnet(ridge, s=50, type="coefficients")[1:10]

#### PREDICTS THE Y'S FOR NEW VALUES OF X GIVEN LAMBDA
pred.y = predict.glmnet(ridge, s=1, newx = x.test)


						#### MODEL COMPARISON ####

k = 10
folds = sample(1:k, nrow(x.train), replace = TRUE)
cv.errors = matrix(NA, k, 100)	# ridge
cv.errors2 = matrix(NA, k, 100)  # lasso
lam = 10^seq(10, -2, length=100)
df = rep(0, length(lam))
df.ortho = rep(0, length(lam))  # divide OLS coefficients by 1+lambda
shrink = rep(0, length(lam))  # shrinkage factor for lasso
d = svd(x.train)$d


#### COMPUTE TEST ERROR FOR EVERY VALUE OF LAMBDA AT EVERY FOLD
for(i in 1:k)
{
	ridge.coefs = glmnet(x.train[folds!=i,], y.train[folds!=i], alpha=0, lambda=lam)
	lasso.coefs = glmnet(x.train[folds!=i,], y.train[folds!=i], alpha=1, lambda=lam)
	
	for(l in 1:length(lam))
	{
		pred = predict.glmnet(ridge.coefs, s=lam[l], newx=x.train[folds==i,])  # ridge
		pred2 = predict.glmnet(lasso.coefs, s=lam[l], newx=x.train[folds==i,])  # lasso
		
		cv.errors[i,l] = mean((y.train[folds==i] - t(pred))^2)
		cv.errors2[i,l] = mean((y.train[folds==i] - t(pred2))^2)
		
		# compute degrees of freedom for ridge
		if(i == 1)
		{
			for(j in 1:(ncol(x.train) - 1))
			{
				df[l] = df[l] + d[j]^2 / (d[j]^2 + lam[l])
				df.ortho[l] = df.ortho[l] + 1 / (1 + lam[l])
			}
		}
		
		# compute shrinkage factor for lasso
		shrink[l] = sum(lasso.coefs$beta[,l]) / sum(lasso.coefs$beta[,100])
	}
}

#### AVERAGE OVER FOLDS (COLUMNS OF CV.ERROR) AND COMPUTE STANDARD ERROR
std <- function(x) sqrt(var(x)/length(x))

stderr = apply(cv.errors, 2, std)
stderr2 = apply(cv.errors2, 2, std)
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors2 = apply(cv.errors2, 2, mean)

par(mfrow=c(2,3), cin=1.5, cra=1.5)


	#### OLS TEST ERROR AND BOOTSTRAPPED STANDARD ERROR
ols = lm(prostate[prostate$train==T,]$lpsa~., data=prostate[prostate$train==T,])
ols.pred = predict.lm(ols, prostate[prostate$train==F,], interval="predict")
ols.error = mean((y.test - ols.pred[,1])^2)  # 0.521274

x <- prostate[prostate$train==T,][,-10]
B=1000
bootstrap.ests = matrix(NA, 1, B)
for(i in 1:B)
{
	samples <- x[sample(nrow(x), size=nrow(x), replace=TRUE),]  # compute bootstrap sample
	ols.boot = lm(samples$lpsa~., data=samples)  # fit linear model on the sample
	ols.pred.boot = predict.lm(ols.boot, prostate[prostate$train==F,], interval="predict")  # 
	bootstrap.ests[i] = mean((y.test - ols.pred.boot[,1])^2)
}
ols.stderr = (1/(B-1)) * sum((bootstrap.ests-mean(bootstrap.ests))^2)  # 0.011


	#### PCR PLOT
pcrtrain <- prostate[prostate$train==T,][,-10]
y <- pcrtrain$lpsa
pcr.coefs = pcr(y~., data=pcrtrain, scale=TRUE, validation="CV")
validationplot(pcr.coefs, val.type="MSEP", main="Principal Components Regression", cex=2, type="b")

summary(pcr.coefs)


	#### RIDGE TEST ERROR VS DEGREES OF FREEDOM
#### ASSUMING ORTHOGONAL X
plot(df.ortho, mean.cv.errors, xlim=c(0,8), ylim=c(0.5, 1.8), type="b", ylab="ridge test error (assuming orthogonal X matrix)", xlab="degrees of freedom", main="Ridge Regression", cex=2)
errbar(df.ortho, mean.cv.errors, yplus=mean.cv.errors+stderr, yminus=mean.cv.errors-stderr, add=TRUE)


#### FIND BEST MODEL ACCORDING TO ONE-STANDARD-DEV RULE:
ridge.best = bestModel(df.ortho, mean.cv.errors, stderr)  # 4.825485 degrees of freedom --> lambda = lam[85] = 0.6579332

#### NOT ASSUMING ORTHOGONAL X
plot(df, mean.cv.errors, xlim=c(0,8), ylim=c(0.5, 1.8), type="b", ylab="ridge test error", xlab="degrees of freedom", main="Ridge Regression", cex=2)
errbar(df, mean.cv.errors, yplus=mean.cv.errors+stderr, yminus=mean.cv.errors-stderr, add=TRUE)



	#### LASSO TEST ERROR VS SHRINKAGE FACTOR
plot(shrink, mean.cv.errors2, xlim=c(0,1), ylim=c(0.5, 1.8), type="b", ylab="lasso test error (assuming orthogonal X matrix)", xlab="shrinkage factor", main="Lasso", cex=2)
errbar(shrink, mean.cv.errors2, yplus=mean.cv.errors2+stderr2, yminus=mean.cv.errors2-stderr2, add=TRUE)


#### FIND BEST MODEL ACCORDING TO ONE-STANDARD-DEV RULE:
lasso.best = bestModel(shrink, mean.cv.errors2, stderr2)  # 0.7023819 shrinkage factor --> lambda = lam[90] = 0.1629751



	#### COEFFICIENTS VS DEGREES OF FREEDOM
#### RIDGE
ridge.coefs = glmnet(x.train, y.train, alpha=0, lambda=lam)

plot(df.ortho, ridge.coefs$beta[1,], type="b", col="blue", xlim=c(0,8.6), ylim=c(-0.3, 0.8), xlab="degrees of freedom (assuming orthogonal X matrix)", ylab="ridge coefficients", cex=2, main="Ridge Coefficients")
abline(v=ridge.best[1], lty=2)
text(x=1.1*ridge.best[1], -0.2, labels="Best Model")

for(p in 2:(nrow(ridge.coefs$beta)-1))
{
	lines(df.ortho, ridge.coefs$beta[p,], type="b", col="blue", cex=2)
}
text(x=8.5, y=ridge.coefs$beta[,100], labels=rownames(ridge.coefs$beta), cex=1.2)


#### LASSO 
lasso.coefs = glmnet(x.train, y.train, alpha=1, lambda=lam)

plot(shrink, lasso.coefs$beta[1,], type="b", col="blue", xlim=c(0,1.1), ylim=c(-0.3, 0.8), xlab="shrinkage factor", ylab="lasso coefficients", cex=2, main="Lasso Coefficients")
abline(v=lasso.best[1], lty=2)
text(x=1.1* lasso.best[1], -0.2, labels="Best Model")

for(p in 2:(nrow(lasso.coefs$beta)-1))
{
	lines(shrink, lasso.coefs$beta[p,], type="b", col="blue", cex=2)
}
text(x=1.05, y=lasso.coefs$beta[,100], labels=rownames(lasso.coefs$beta), cex=1.2)




						#### COMPUTING TEST ERROR AND BETA VECTOR FOR BEST MODELS SELECTED
						
	#### RIDGE TEST ERROR
ridge = glmnet(x.train, y.train, alpha=0, lambda=lam)
coef(ridge)[,ridge.best[2]] # beta vector
pred.y = predict.glmnet(ridge, s=lam[ridge.best[2]], newx = x.test)
ridge.error = mean((y.test-pred.y)^2)  # 0.503997

#### BOOTSTRAPPED STD ERROR FOR RIDGE
x = as.matrix(prostate[prostate$train==T,][, -10])
test = as.matrix(prostate[prostate$train==F,][, -10])
B=1000
bootstrap.ests = matrix(NA, 1, B)
for(i in 1:B)
{
	samples <- x[sample(nrow(x), size=nrow(x), replace=TRUE),]  # compute bootstrap sample
	ridge.boot = glmnet(samples, samples[,9], alpha=0, lambda=lam)  # fit linear model on the sample
	ridge.pred.boot = predict.glmnet(ridge.boot, s=lam[ridge.best[2]], newx = test)  # 
	bootstrap.ests[i] = mean((y.test - ridge.pred.boot[,1])^2)
}
ridge.stderr = (1/(B-1)) * sum((bootstrap.ests-mean(bootstrap.ests))^2)


	#### LASSO TEST ERROR
lasso = glmnet(x.train, y.train, alpha=1, lambda=lam)
coef(lasso)[,lasso.best[2]] # beta vector
pred.y2 = predict.glmnet(lasso, s=lam[lasso.best[2]], newx = x.test)
lasso.error = mean((y.test-pred.y2)^2)  # 0.4596582

#### BOOTSTRAPPED STD ERROR FOR LASSO
x = as.matrix(prostate[prostate$train==T,][, -10])
test = as.matrix(prostate[prostate$train==F,][, -10])
B=1000
bootstrap.ests = matrix(NA, 1, B)
for(i in 1:B)
{
	samples <- x[sample(nrow(x), size=nrow(x), replace=TRUE),]  # compute bootstrap sample
	lasso.boot = glmnet(samples, samples[,9], alpha=1, lambda=lam)  # fit linear model on the sample
	lasso.pred.boot = predict.glmnet(lasso.boot, s=lam[lasso.best[2]], newx = test)  # 
	bootstrap.ests[i] = mean((y.test - lasso.pred.boot[,1])^2)
}
lasso.stderr = (1/(B-1)) * sum((bootstrap.ests-mean(bootstrap.ests))^2)




#### PROBLEMS:
# - wasn't able to use predict function for pcr because of "variable lengths differ" error
# - not sure if bootstrap was done correctly
# - did not get correlation thing



# library(ISLR)
# fix(Hitters)
# library(pls)

# pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
# # summary(pcr.fit)

# x=model.matrix(Salary~., Hitters)[,-1]
# y=Hitters$Salary

# set.seed(1)
# train=sample(1:nrow(x), nrow(x)/2)
# test=(-train)
# y.test=y[test]


# pcr.pred=predict(pcr.fit, Hitters[test,], ncomp=7)



