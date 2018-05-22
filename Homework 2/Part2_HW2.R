#spindleupmybutt

######## PART 2 HW 2

#### 1) logistic regression model
library(alr4)
titanic = read.table("titanic.txt", header = T)


#### remove missing values for ungrouped dataset. We assume that the missing values are a random sample of the original set
titanic3 = read.csv("titanic3.1.csv", header = T, skipNul = T)
titanic4 <- titanic3[c("pclass", "survived", "sex", "age")]
sum(is.na(titanic4))
titanic2 <- na.omit(titanic4)

# make a logistic model
classf <- factor(titanic$class)
agef <- factor(titanic$age)
sexf <- factor(titanic$sex)

logModel1 <- glm(cbind(surv, m-surv)~classf+agef+sexf, family=binomial, data=titanic)
summary(logModel1)

logModel1u <- glm(survived~pclass+age+sex, family=binomial, data=titanic2)
summary(logModel1u)

# obtain the probabilities for each combination of predictors from the model
eff1 = Effect(c("classf", "agef", "sexf"), logModel1)

 # classf*agef*sexf effect
# , , sexf = female

              # agef
# classf       adult     child
  # crew   0.7660538        NA
  # first  0.8853234 0.9571141
  # second 0.7360897 0.8896612
  # third  0.5661291 0.7904463

# , , sexf = male

              # agef
# classf       adult     child
  # crew   0.2254997        NA
  # first  0.4070382 0.6649249
  # second 0.1987193 0.4175655
  # third  0.1039594 0.2511586

x <-subset(eff1, sexf == "female")
plot(eff1)

# compare with the frequencies in the training set
   # surv   m  class   age    sex
# 1    20  23   crew adult female
# 2   192 862   crew adult   male
# 3     1   1  first child female
# 4     5   5  first child   male
# 5   140 144  first adult female
# 6    57 175  first adult   male
# 7    13  13 second child female
# 8    11  11 second child   male
# 9    80  93 second adult female
# 10   14 168 second adult   male
# 11   14  31  third child female
# 12   13  48  third child   male
# 13   76 165  third adult female
# 14   75 462  third adult   male



#### grouped data
adult <- subset(titanic, age == "adult")
child <- subset(titanic, age == "child")
male <- subset(titanic, sex == "male")
female <- subset(titanic, sex == "female")

maleAdult <- subset(adult, sex == "male")
maleChild <- subset(child, sex == "male")
femaleAdult <- subset(adult, sex == "female")
femaleChild <- subset(child, sex == "female")


par(mfrow = c(2,2))
#### grouped
plot(I(surv/m) ~ class, maleAdult, type = "p", main = "male adult")
plot(I(surv/m) ~ class, maleChild, type = "p", main = "male child")
plot(I(surv/m) ~ class, femaleAdult, type = "p", main = "female adult")
plot(I(surv/m) ~ class, femaleChild, type = "p", main = "female child")


# Below are shown 4 separate tables where each corresponds to a combination of sex and age, and the ratios of surv vs m against class for each table. The plots are shown in Figure [ ]. The predicted values for each combination of predictors are represented in the Effects plot in Figure [ ]. By visual comparison, the predicted values don't match the prior probabilities very well, except for adult females. Also, in the frequencies plot in Figure [ ], we see that sex and age are noticeably correlated: there is a difference in the way sex affects the frequency of survival when age is varied, holding class fixed. For children, sex doesn't affect survival at all, but for adults it does. The range of survival frequencies for adult males was 0.1 to 0.35 whereas for adult females it was 0.45 to 0.95. Therefore, this model isn't very adequate without at least one interaction parameter.

summary(logModel2)

#### interaction plots. They clearly show an interaction between class & age, age & sex
par(mfrow = c(2,2))
interaction.plot(titanic$class, titanic$age, titanic$surv/titanic$m, type="l")
interaction.plot(titanic$class, titanic$sex, titanic$surv/titanic$m, type="l")
interaction.plot(titanic$age, titanic$sex, titanic$surv/titanic$m, type="l")

#### 2) adding interaction terms: class & sex, class & age, sex & age
classbf <- classf[3:14]

#### logistic model with all interaction terms
logModel2 <- glm(cbind(surv, m-surv)~classf+agef+sexf+classf*sexf+classf*agef+sexf*agef, family=binomial, data=titanic)
anova(logModel2)
Anova(logModel2, type=2)
anova(logModel1)
step(logModel2)

avPlots(logModel2u)

#### An ANOVA test on logModel2 shows that removing the age:sex interaction term from the model with all two-way interactions results in a residual deviance of 1.69, which still indicates a good fit. Dropping class:age produces a residual deviance of 44.2, and dropping class:sex produces a residual deviance of 66.7. This shows that age:sex doesn't contribute much to the log-likelihood function of the model. The command used was Anova(logModel2, type=2), which computes a Likelihood Ratio Chi-Squared test for each addition of a new predictor, respecting the principle of marginality (adding each predictor before any interaction terms it appears in). The p-value of the LR test of age:sex was 0.1942, which means we can safely reject the alternative hypothesis (the age:sex coefficient is non-zero) at a threshold of 5%.

#### A stepwise comparison of the models was performed using step(logModel2). The results show that dropping age:sex decreases the AIC of the model from 70.6 to 70.3. This isn't a drastic improvement, but it confirms that this interaction term doesn't improve the model significantly.

#### The speculation in part 1 about age and sex being correlated may be justified if the variation in the age:sex interaction term was caused by other variables, making it seem like an important predictor on its own.

logModel3 <- glm(cbind(surv, m-surv)~classf+agef+sexf+classf*sexf+classf*agef, family=binomial, data=titanic)
summary(logModel3)

#### Interpretation:
#### From the estimates obtained in the final model, we see that first class passengers had a log-odds ratio of surviving of 1.658, and those in third class had a log-odds ratio of -2.115. This means that the odds of surviving among first class passengers was 5.25 times that of the other classes, while those of third class were 0.12 times that of the other classes. These odds translate to a 13% chance of survival for third class, and 84% chance for first class passengers, knowing that the prior probability of survival for the entire population was 32%.

logModel2u <- glm(survived~pclass+age+sex+pclass*sex+pclass*age+age*sex, family=binomial, data=titanic2)
logModel3u <- glm(survived~pclass+age+sex+pclass*sex+pclass*age, family=binomial, data=titanic2)

summary(logModel2u)
summary(logModel3u)

#### According to the model, children seem to have been equally likely to survive between first and third classes. The coefficient for the firstclass/child dummy variable has a p-value in excess of 0.99, so does the dummy variable for secondclass/child. This is confirmed in an ANOVA test using the ungrouped data: removing the class:age variable from the model results in a deviance of only 0.483. Why then was class:age a meaningful predictor with the ungrouped dataset? This is because in the ungrouped dataset, age is a continuous variable, whereas it was a categorical variable in the grouped dataset. While in both the grouped and ungrouped dataset the coefficients were close to zero and their p-values were high (0.998 and 0.488 resp.), keeping the interaction between class and age in the grouped dataset yields a better model fit. Looking at the prior probabilities shows clearly that children in third class had a much lower survival rate than the rest, but this pattern isn't captured by the model.


# • explain adequacy of model by looking at the data first (question 1)
# read about chi-squared, central limit theorem, F-distribution, anova in depth
# do anova test and decide if any predictors can be removed.
## work with ungrouped data
# when you compare marginal plots with added variable plots for variable X, what does a more spread out added variable plot mean? it means that other variables are contributing the the variation in X, and that X itself doesn't contribute much to the variation in the output variable.
# rescale the betas when comparing them with each other (multiply by std error of predictor)
# understand F-test and p-values well

# anova compares two models, usually with a difference in some variable in whose effect on the model you want to study. anova(model1, model2) assumes that model1 is included in model2 (very often model1 is the null hypotheses and model2 is Ha). It calculates the f-value (RSS1-RSS2)/(df1-df2)//RSS2/df2 of the two models being compared. depending on how the software is implemented, anova(model) sequentially chooses a variable and adds it, and compares the models pairwise according to some order.

# the added variable plot isolates the effect of X on Y
# marginal plot ignores the effect of X on y
# effects plot fixes the rest at some value and evaluates Y wrsp to X.

titanic3 = read.table("titanic3.1.xls", header = T, na.strings = "NA")

###################################################################################
###################################################################################
###################################################################################


library(alr4)

head(BlowBS)
head(Blowdown)


#blowBS subset containing only the black spruce trees and using covariate classes  based on d, so that data become binomial
#if data binomial the response  to use is  Y=(success, failures)
#in BLowdown the response is 0 =survived, 1 =died


g1 <- glm(cbind(died, m-died) ~ log(d), family=binomial, data=BlowBS)

plot( I(died/m) ~ d, BlowBS,  cex=.4*sqrt(m), log="x", ylim=c(0,1))

#add the fitted curve logistic(beta_0+beta_1log(d)))
dnew <- seq(3, 40, length=100)
lines(dnew, predict(g1, newdata=data.frame(d=dnew), type="response"), lwd=1.5)
grid(col="gray", lty="solid")



#using instead the original data and second order model
g2 <- glm(y ~ (log(d) + s)^2, binomial, Blowdown, subset=spp=="balsam fir")
#marginal plots to check accuracy
mmps(g2)



e2 <- Effect(c("d", "s"), g2, xlevels=list(s=c(.2, .35, .55)))
plot(e2, multiline=TRUE, grid=TRUE, xlab="Diameter",main="", ylab="Prob. of blow down",
rescale.axis=FALSE, rug=FALSE, key.args=list(corner=c(.02,.98)))



g3 <- glm(y ~ (log(d) + s + spp)^3 , binomial, Blowdown)
Anova(g3)

e3 <- Effect(c("d", "s","spp"), g3, xlevels=list(s=c(.2, .35, .55 )))
plot(e3, multiline=TRUE, grid=TRUE, xlab="D",main="", lines=c(1, 3, 2), rotx=45,
ylab="Prob", rescale.axis=FALSE, rug=FALSE)
