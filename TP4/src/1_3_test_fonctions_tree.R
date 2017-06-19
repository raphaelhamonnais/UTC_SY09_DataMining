source("src/fonctions/tree.R")

##############Test#######################
appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]

testData <- read.csv("data/Synth1-40.csv")
Xtst <- testData[,1:2]
ztst <- testData[,3]

out = tree.app(Xapp,zapp,Xtst)
out$pred
1 - compute.sucess.rate(out$pred, ztst)
