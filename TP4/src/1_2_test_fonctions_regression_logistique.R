#install.packages("MASS")
library("MASS")

source("src/fonctions/functions.util.R")
source("src/fonctions/logistic.R")
source("src/fonctions/logistic_quadratic.R")
source("src/fonctions/prob.log.R")
source("src/fonctions/prob.log2.R")

appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]

testData <- read.csv("data/Synth1-40.csv")
Xtst <- testData[,1:2]
ztst <- testData[,3]

# avec intercept
log1 <- log.app(Xtst, ztst, T, 1e-5)
log1$beta
log1$iter
log1$logL

# sans intercept
log2 <- log.app(Xtst, ztst, F, 1e-5)
log2$beta
log2$iter
log2$logL

prediction = log.val(log1$beta,Xtst)
1 - compute.sucess.rate(prediction$pred, ztst)

prob.log(log2$beta, Xtst, ztst, seq(0,1,0.25))



##################### Test log quadratique ########################################


appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]

# situation avec le coordonnée d'origine 
log_quad1 <- log_quad.app(Xapp, zapp, T, 1e-5)
prediction = log_quad.val(log_quad1$beta, Xapp)
1 - compute.sucess.rate(prediction$pred, ztst)
prob.log2(log_quad1$beta, Xapp, zapp, seq(0,1,0.25))

# situation sans le coordonnée d'origine 
log_quad2 <- log_quad.app(Xtst, ztst, F, 1e-5)
prob.log2(log_quad2$beta, Xapp, zapp, seq(0,1,0.25))