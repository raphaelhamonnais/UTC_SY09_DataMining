#install.packages("MASS")
library("MASS")

source("src/fonctions/functions.util.R")
source("src/fonctions/anadisc.R")
source("src/fonctions/mvdnorm.R")
source("src/fonctions/prob.ad.R")
source("src/fonctions/separ1.R")

########### Tests des fonctions ###########
appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]

testData <- read.csv("data/Synth1-1000.csv")
Xtst <- testData[,1:2]
ztst <- testData[,3]


# test adq.app
params = adq.app(Xapp,zapp)
params$pi
params$mu
params$sigma
prob.ad(params, Xapp, zapp, seq(0, 1, 0.25))

# test adl.app
params = adl.app(Xapp,zapp)
params$pi
params$mu
params$sigma
prob.ad(params, Xapp, zapp, seq(0, 1, 0.25))

# test nba.app
params = nba.app(Xapp,zapp)
params$pi
params$mu
params$sigma
prob.ad(params, Xapp, zapp, seq(0, 1, 0.25))

# test ad.val
params = adq.app(Xapp, zapp)
val = ad.val(params,Xapp)
prediction_and_posteriori_prob = cbind(round(t(t(val$pw1)), 3), round(t(t(val$pw2)), 3), t(t(val$pred)))
prediction_and_posteriori_prob

