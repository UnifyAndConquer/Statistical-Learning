################################ CHAPTER 4 LAB

#### correlations between lag1 -> lag5 
cor(Smarket [,-9])

#### logistic regression of direction against lags & volume
test = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Smarket)
summary(model1 )


################################ HW2 PART 1

#### List of figures:
# Figure 1: Comparison of responses of both models for selected predictor values
# Figure 2: telemetry data from NASA showing internal pressures of both SRBs during the flight

#### QUESTON 1:

# A logistic model was used to predict the probability of a failure as a function of temperature and pressure. The response is modified to be true if at least one o-ring failed, and false otherwise. 

train = read.table("Challenger.txt", header = T)
train

#### make failure a true/false variable (true if at least one o-ring failed)
bFail = replace(train$fail, cbind(14, 21), 1)
bFail

logReg1 = glm(bFail ~ temp, data = train, family = binomial)
logReg2 = glm(bFail ~ temp + pres, data = train, family = binomial)
summary(logReg1)
summary(logReg2)

Anova(logReg2, type=2)
anova(logReg2)
AIC(logReg2)

####according to NASA telemetry data, pressure was 610 psi at time of o-ring failure
predict(logReg1, data.frame(temp = 31), type = "response")
predict(logReg2, data.frame(temp = 31, pres = 610), type = "response")

# logReg1 is the model with only temperature as a predictor, and logReg2 the one with both temperature and pressure. For logReg1 the estimator was -0.2322 ± 0.1082, which means for every unit increase in temperature, the response decreases by 0.2322, which means the odds of failure get divided by about 1.25. For logReg 2 the estimate was -0.2415 ± 0.1097.

# There is little difference between both models in terms of deviance and temperature estimates. This shows that pressure doesn't have a strong effect on the model's fit. The responses for some relevant temperatures and pressures are shown in Figure 1: the estimated probability of a failure is 0.999997 accoring to logReg2, and 0.999609 as predicted by logReg1. According to NASA telemetry data, the internal pressure of the right SRB (the one that got fractured and caused the explosion) was 610 psi just before the pressure started to drop abnormally. However, even with 50 psi as a predictor, the estimated probability is still 0.999357.

#### QUESTON 2:

train2 = read.table("ChallengerUngrouped.txt", header = T)

library(MASS)

lda1 = lda(fail ~ temp + pres, data = train2)
lda1
predict(lda1, data.frame(temp = 31, pres = 610), type = "response")


qda1 = qda(fail ~ temp + pres, data = train2)
qda1
predict(qda1, data.frame(temp = 31, pres = 610), type = "response")

library(class)

train2.data = cbind(train2$temp, train2$pres)
train2.response = train2$fail
test.data = data.frame(temp = 31, pres = 610)

set.seed(1)
knn1 = knn(train2.data, test.data, train2.response, k=1)
knn1

