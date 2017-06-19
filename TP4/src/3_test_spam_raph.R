library("MASS")
source("src/fonctions/prob.ad.R")
source("src/fonctions/separ1.R")
source("src/fonctions/mvdnorm.r")
source("src/fonctions/anadisc.R")
source("src/fonctions/logistic.R")
source("src/fonctions/logistic_quadratic.R")
source("src/fonctions/tree.R")
source("src/fonctions/functions.util.R")



appData <- read.csv("data/spam.csv")
appData = appData[,-1]


zIndex = dim(appData)[2]
X <- appData[,1:zIndex-1]
Z <- appData[,zIndex]
ACP = prcomp(X, center = F, scale = F)
plot(ACP)
dataACP = ACP$x
plot(dataACP[,1:2], col = Z)#, xlim = c(-1000, 1000))
inertieExplique = round(ACP$sdev / sum(ACP$sdev) * 100, 4)
inertieExpliqueCum = vector(length = length(inertieExplique))
for (i in 1:length(inertieExplique)) {
    inertieExpliqueCum[i] = sum(inertieExplique[1:i])
}
inertieExpliqueCum

#ACP_prin = princomp(X, cor = FALSE)
#plot(ACP_prin)
#ACP_prin$scores
#plot(ACP_prin$scores[,1:2], col = Z)#, xlim = c(-1000, 1000))




intr = T
epsi = 1e-5

for (i in 1:20) {
    writeLines("-----------")
    cat("Iteration ", i, "\n")
    writeLines("-----------")
    sample = separ1(dataACP[,1:2], Z)
    params = adq.app(sample$Xapp, sample$zapp) # calculer les paramètres du modèle
    testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
    error = 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
    prob.ad(params, sample$Xtst, sample$ztst, seq(0, 1, 0.25))
    print(error)
    writeLines("")
    writeLines("")
    writeLines("")
}


for (i in 1:20) {
    writeLines("-----------")
    cat("Iteration ", i, "\n")
    writeLines("-----------")
    sample = separ1(dataACP[,1:2], Z)
    params = adl.app(sample$Xapp, sample$zapp)
    testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
    1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
    prob.ad(params, sample$Xtst, sample$ztst, seq(0, 1, 0.25))
    print(error)
    writeLines("")
    writeLines("")
    writeLines("")
}


for (i in 1:20) {
    writeLines("-----------")
    cat("Iteration ", i, "\n")
    writeLines("-----------")
    sample = separ1(dataACP[,1:2], Z)
    params = nba.app(sample$Xapp, sample$zapp)
    testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
    1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
    prob.ad(params, sample$Xtst, sample$ztst, seq(0, 1, 0.25))
    print(error)
    writeLines("")
    writeLines("")
    writeLines("")
}

for (i in 1:20) {
    writeLines("-----------")
    cat("Iteration ", i, "\n")
    writeLines("-----------")
    sample = separ1(dataACP[,1:2], Z)
    params = log.app(sample$Xapp, sample$zapp, intr, epsi)
    testPredictedClasses = log.val(params$beta,sample$Xtst)
    1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
    prob.log(params$beta, sample$Xtst, sample$ztst, 0.5)
    print(error)
    writeLines("")
    writeLines("")
    writeLines("")
}


for (i in 1:20) {
    writeLines("-----------")
    cat("Iteration ", i, "\n")
    writeLines("-----------")
    sample = separ1(dataACP[,1:2], Z)
    params = log_quad.app(sample$Xapp, sample$zapp, intr, epsi)
    testPredictedClasses = log_quad.val(params$beta, sample$Xtst)
    1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
    prob.log2(params$beta, sample$Xtst, sample$ztst, 0.5)
    print(error)
    writeLines("")
    writeLines("")
    writeLines("")
}

