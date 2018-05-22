################################ CHAPTER 4 LAB

#### correlations between lag1 -> lag5 
cor(Smarket [,-9])

#### logistic regression of direction against lags & volume
test = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Smarket)
summary(model1 )


################################ HW2 PART 1

challenger = read.table("Challenger.txt", header = T)

#### make failure a true/false variable (true if at least one o-ring failed)
bFail = replace(data$fail, cbind(14, 21), 1)

logReg1 = glm(bFail ~ temp, data = challenger, family = binomial)
summary(logReg1)