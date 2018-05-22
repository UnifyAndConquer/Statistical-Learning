########## PART 2 ##########

library(alr4)
####	read data from nyc.csv
resdata <- read.csv(file="nyc.csv", header=T)

####	build linear model from original variables
model1 = lm(Price ~ Food + Service + Decor + East, data = resdata)
summary(model1)

####	pairs plot, looks good
pairs(~ Price + Food + Service + Decor + East, resdata)

####	try power transform
pow <- powerTransform(cbind(Price, Food, Service, Decor, factor(East)) ~ 1, data = resdata)

####	don't have to transform binary values
summary(pow)
summary(resdata)

####	residuals seem OK
par(mfrow=c(1,1))
plot(model1, 1)
abline(0,0)

####	check outliers, get index from residual plot
influenceIndexPlot(model1) 
plot(model1, 1)		#points 56, 30 and 130

####	remove outliers
model2b = lm(Price ~ Food + Service + Decor + East, data = resdata, subset = -c(56, 30, 130))
plot(predict(model2), rstudent(model2))
abline(0,0)
summary(model1)

####	service doesn't seem to be an important predictor because it 
####	has p-value 0.9945. So we remove it.
model2 = lm(Price ~ Food + Decor + East, data = resdata)

####	draw added variable plots just to make sure
par(mfrow = c(2,2))
avPlot(model1, "Food") 
avPlot(model1, "Decor")
avPlot(model1, "East")
avPlot(model1, "Service")

par(mfrow = c(2,2))
avPlot(model2, "Food") 
avPlot(model2, "Decor")
avPlot(model2, "East")


####	add interaction terms. center first? reindtroduce Service
####	after adding interaction terms
Food_East = (resdata$Food - mean(resdata$Food)) * (resdata$East - mean(resdata$East))
Decor_East = (resdata$Decor - mean(resdata$Decor)) * (resdata$East - mean(resdata$East))
Service_East = (resdata$Service - mean(resdata$Service)) * (resdata$East - mean(resdata$East))

model3 = lm(Price ~ Food + Decor + East + Service + Food_East + Decor_East + Service_East, data = resdata)

par(mfrow = c(2,3))
avPlot(model3, "Food") 
avPlot(model3, "Decor")
avPlot(model3, "East")
avPlot(model3, "Decor_East")
avPlot(model3, "Food_East")
avPlot(model3, "Service_East")

summary(model3)
# not much better than the old model

