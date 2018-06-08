#### STAT 239 HOMEWORK 4 DATA ANALYSIS 3 ####

#### K-MEANS CLUSTERING ####
library(scatterplot3d)
library(randomForest)

set.seed(42)
train = read.table("test.txt", header = T)
X = model.matrix(y~., data = train)
Y = train$y - 1

km.model = kmeans(X, 2, nstart = 50)
c = km.model$cluster - 1
colors = ifelse(c, "red", "blue")

# 3d plot of first three principal components for visualization
pcr.model = prcomp(X, retx = TRUE)
summary(pcr.model)
PC1 = pcr.model$x[,"PC1"]
PC2 = pcr.model$x[,"PC2"]
PC3 = pcr.model$x[,"PC3"]

scatterplot3d(PC1, PC2, PC3, color = colors, main = "k-means clustering with two classes", type="h", angle = 70)
plot(x=PC1, y=PC2,  ylab = "PC2", col = colors, main = "k-means clustering with two classes")

# let's find out if the clusters correspond to the y values
km.err = sum(Y != c) # 7! pretty impressive


#### HIERARCHICAL CLUSTERING ####
# euclidean distance
hc.model = hclust(dist(X), method="complete")
plot(hc.model, cex=0.5, main="complete linkage and euclidean distance")

# random forest distance
rf.model = randomForest(X, proximity = TRUE, importance = TRUE)
rf.prox = rf.model$proximity

hc.model2 = hclust(as.dist(rf.prox), method="complete")
plot(hc.model2, cex=0.5, main="complete linkage and RF distance")

# correlation-based distance
cor = cor(X)[2:22, 2:22]
hc.model3 = hclust(as.dist(cor), method="complete")
plot(hc.model3, cex=1, main="complete linkage and correlation-based distance")


