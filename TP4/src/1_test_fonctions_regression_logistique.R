#install.packages("MASS")
library("MASS")

source("src/fonctions/functions.util.R")
source("src/fonctions/logistic.R")
source("src/fonctions/prob.log.R")

appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]

testData <- read.csv("data/Synth1-40.csv")
Xtst <- testData[,1:2]
ztst <- testData[,3]

log1 <- log.app(Xtst, ztst, T, 1e-5)
log1$beta
log1$iter
log1$logL


log2 <- log.app(Xtst, ztst, F, 1e-5)
log2$beta
log2$iter
log2$logL

prediction = log.val(log1$beta,Xtst)
prediction$prob
prediction$pred
rate = compute.sucess.rate(prediction$pred, ztst)
rate
prob.log(beta, Xtst, ztst, 0.5)
