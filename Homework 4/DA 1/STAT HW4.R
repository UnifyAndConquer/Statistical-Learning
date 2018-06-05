#### STAT 239 HW 4

#### DATA ANALYSIS I: BAGGING LINEAR PREDICTORS

# 1)
library(MASS)
library(leaps)

n = 60
p = 30
rho = 0.4

betaZero_1 = 3.6
beta_1 = c(round(rnorm(5, 0, 2), 2), rep(0, 25))
 # [1]  1.90  2.27 -4.27  1.85 -2.40  0.00  0.00  0.00  0.00  0.00  0.00  0.00
# [13]  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
# [25]  0.00  0.00  0.00  0.00  0.00  0.00

betaZero_2 = -2.3
beta_2 = c(round(rnorm(25, 0, 2), 2), rep(0, 5))
 # [1]  0.48 -2.16  0.77 -0.23  0.50 -2.19 -0.77  1.94  1.21  0.48  1.20 -2.09
# [13]  0.29 -0.87  1.35 -2.62 -1.48 -1.75 -1.15 -2.74  0.19 -4.59 -0.61  0.36
# [25] -0.33  0.00  0.00  0.00  0.00  0.00

sigma = matrix(ncol = p, nrow = p, 0)
for(i in 1:p)
{
	for(j in 1:i)
	{
		sigma[i, j] = rho^{abs(i-j)}
		sigma[j, i] = sigma[i, j]
	}
}

predict.regsubsets = function(object, newdata , id) 
{
	if(object$nvmax <= id) {print(object$nvmax);  return(NA)}
	p = ncol(newdata)
	mat = cbind(rep(1, nrow(newdata)), newdata)
	colnames(mat) = c("(Intercept)", 1:p)
	coeffic = coef(summary(object)$obj, id=id)
	xvars = names(coeffic)
	mat[ , xvars]%*% coeffic
}

M = 300
s1 = rep(NA, M)
s2 = rep(NA, M)

err_1 = matrix(NA, nrow=M, ncol=p)
err_2 = matrix(NA, nrow=M, ncol=p)
err_1b = matrix(NA, nrow=M, ncol=p)
err_2b = matrix(NA, nrow=M, ncol=p)

e_1 = rep(NA, p)
e_2 = rep(NA, p)
e_1b = rep(NA, p)
e_2b = rep(NA, p)

for(m in 1:M)
{
	X_Train = mvrnorm(n = 60, mu = rep(0, p), sigma)
	X_Test = mvrnorm(n = 60, mu = rep(0, p), sigma)
	
	Y_Train_1 = betaZero_1 + X_Train %*% beta_1 + rnorm(n, 0, 1)
	Y_Train_2 = betaZero_2 + X_Train %*% beta_2 + rnorm(n, 0, 1)
	
	Y_Test_1 = betaZero_1 + X_Test %*% beta_1 + rnorm(n, 0, 1)
	Y_Test_2 = betaZero_2 + X_Test %*% beta_2 + rnorm(n, 0, 1)
	
	# signal to noise ratio
	s1[m] = t(beta_1) %*% cov(X_Train) %*% beta_1
	s2[m] = t(beta_2) %*% cov(X_Train) %*% beta_2
	
	# 2)
	model_1 = regsubsets(x = X_Train, y = Y_Train_1, method = "forward", nvmax = p)
	model_2 = regsubsets(x = X_Train, y = Y_Train_2, method = "forward", nvmax = p)
	
	pred_1 = matrix(ncol = p, nrow = n, 0)
	pred_2 = matrix(ncol = p, nrow = n, 0)
	
	error_1 = rep(0, p)
	error_2 = rep(0, p)
	
	for(k in 1:p)
	{
		pred_1[,k] = predict(model_1, X_Test, id=k)
		pred_2[,k] = predict(model_2, X_Test, id=k)
		
		error_1[k] = apply((pred_1[,k]-Y_Test_1)^2, 2, mean)
		error_2[k] = apply((pred_2[,k]-Y_Test_2)^2, 2, mean)
	}
	err_1[m,] = error_1
	err_2[m,] = error_2
	
	
	# 3, 4, 5)
	B = 50	
	error_1b = rep(NA, p) 							# bagged errors
	error_2b = rep(NA, p)
	
	pred_1 = array(NA, c(n, p, B)) 					# predicted values array for all bootstrap samples
	pred_2 = array(NA, c(n, p, B))
	pr_1 = matrix(ncol = p, nrow = n, NA) 			# bagged predicted values
	pr_2 = matrix(ncol = p, nrow = n, NA) 
	
	
	for(b in 1:B)
	{
		bootstraps = sample(nrow(X_Train), nrow(X_Train), replace=TRUE)
		model_1 = regsubsets(x=X_Train[bootstraps,], y=Y_Train_1[bootstraps], method="forward", nvmax=p)
		model_2 = regsubsets(x=X_Train[bootstraps,], y=Y_Train_2[bootstraps], method="forward", nvmax=p)
		
		for(k in 1:p)
		{
			pred_1[,k,b] = predict(model_1, X_Test, id=k)
			pred_2[,k,b] = predict(model_2, X_Test, id=k)
		}
	}
	
	for(k in 1:p)
	{	
		for(d in 1:n)
		{
			pr_1[d,k] = mean(pred_1[d,k,])
			pr_2[d,k] = mean(pred_2[d,k,])
		}
		
		error_1b[k] = mean((pr_1[,k]-Y_Test_1)^2)
		error_2b[k] = mean((pr_2[,k]-Y_Test_2)^2)
	}
	
	err_1b[m,] = error_1b
	err_2b[m,] = error_2b

}

for(k in 1:p)
{	
	e_1[k] = mean(err_1[,k])
	e_2[k] = mean(err_2[,k])
	
	e_1b[k] = mean(err_1b[,k])
	e_2b[k] = mean(err_2b[,k])
}

# > e_1
 # [1] 9.984326 2.381089 1.068652 1.173522 1.249622 1.304927 1.358717 1.411865 1.462162
# [10] 1.506995 1.552049 1.599961 1.647065 1.693541 1.729477 1.768453 1.797534 1.829868
# [19] 1.861186 1.893250 1.923851 1.951514 1.971009 1.989775 2.004688 2.017361 2.026766
# [28] 2.033595 2.038020 2.040044
# > e_2
 # [1] 75.432264 64.473354 55.375763 47.215944 39.244337 32.449955 26.651256 21.689624
 # [9] 17.764287 14.521302 11.993148  9.874855  8.141877  6.641746  5.545270  4.642443
# [17]  4.001125  3.485001  3.105679  2.815617  2.563579  2.360525  2.227423  2.117951
# [25]  2.053593  2.044425  2.037062  2.045300  2.050210  2.050096
# > e_1b
 # [1] 7.755886 2.370850 1.070662 1.103037 1.143353 1.182225 1.221932 1.261168 1.300802
# [10] 1.340461 1.381507 1.422694 1.465891 1.512140 1.556010 1.601931 1.648692 1.696246
# [19] 1.744833 1.792589 1.840939 1.889610 1.937979 1.980840 2.022910 2.061350 2.095492
# [28] 2.130417       NA       NA
# > e_2b
 # [1] 60.846133 47.229784 37.882530 30.895538 25.514545 21.217482 17.637653 14.749148
 # [9] 12.354086 10.415506  8.805084  7.477230  6.392901  5.486920  4.733908  4.110233
# [17]  3.590879  3.169489  2.835071  2.566892  2.362739  2.212440  2.117748  2.066348
# [25]  2.046888  2.057313  2.085393  2.117198        NA        NA



pdf("plots.pdf")

plot(e_1, type="l", xlab="# predictors in model", main="5 nonzero coefficients", ylab="test error")
lines(e_1b, type="l", col="blue")
legend(x=20, y=9, legend=c("bagged", "not bagged"), col=c("blue", "black"), lty=1)

plot(e_2, type="l", xlab="# predictors in model", main="25 nonzero coefficients", ylab="test error")
lines(e_2b, type="l", col="blue")
legend(x=20, y=60, legend=c("bagged", "not bagged"), col=c("blue", "black"), lty=1)

dev.off()
