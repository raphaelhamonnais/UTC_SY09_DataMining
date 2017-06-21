library("MASS")
source("src/fonctions/separ1.R")
source("src/fonctions/mvdnorm.r")
source("src/fonctions/anadisc.R")
source("src/fonctions/logistic.R")
source("src/fonctions/logistic_quadratic.R")
source("src/fonctions/tree.rpart.R")
source("src/fonctions/functions.util.R")
source("src/fonctions/prob.ad.R")
source("src/fonctions/prob.log.R")
source("src/fonctions/prob.log2.R")
source("src/fonctions/Fonctions_Voisins.R")
source("src/fonctions/separ2.R")


qdaName = "Quadratic Discriminat Analysis"
ldaName = "Linear Discriminat Analysis"
nbaName = "Naive Bayes classifier"
logName = "Logistic Regression"
logQuadName = "Quadratic Logistic Regression"
decisionTreeName = "Decision Tree"
kppvName = "KPPV"


data <- read.csv("data/spam.csv")
data = data[,-1]
zIndex = dim(data)[2]
X <- data[,1:zIndex-1]
Z <- data[,zIndex]

######### Analyse des variances des données #########
X_high_variance = X[,55:57]
X_low_variance = X[,1:54]
X_high_variance_scaled = X_high_variance / apply(X_high_variance, 2, sd)

diag(var(X_high_variance))
barplot(diag(var(X_high_variance)), ylab = "Variance")
diag(var(X_high_variance_scaled))
barplot(diag(var(X_low_variance)), ylab = "Variance")
diag(var(X_low_variance))


######### Classifieurs sur les données brutes #########
#spamErrorFunction(X, Z, qdaName, 20, printBeforeError = T)
spamErrorFunction(X[,-c(31,32,41)], Z, qdaName, 20, printBeforeError = F) #  Error in chol.default(Sigma) at mvdnorm.r#8 : le mineur dominant d'ordre c(31,32,41) n'est pas défini positif 
spamErrorFunction(X, Z, ldaName, 20)
#spamErrorFunction(X, Z, nbaName, 20, printBeforeError = T)
spamErrorFunction(X[,-c(31,32,41)], Z, nbaName, 20, printBeforeError = F) #  Error in chol.default(Sigma) at mvdnorm.r#8 : le mineur dominant d'ordre c(31,32,41) n'est pas défini positif 
spamErrorFunction(X, Z, logName, 20)
spamErrorFunction(X, Z, decisionTreeName, 20)
displayDecisionTree(X,Z)
spamErrorFunction(X, Z, kppvName, 20)


##### test suppression individus avec f1[i] == 0 && f2[i] == 0 ##########
dim(X)
params = adl.app(X, Z)
f1_ = mvdnorm(X, params$mu[[1]], params$sigma[[1]])
f2_ = mvdnorm(X, params$mu[[2]], params$sigma[[2]])
which(f1_ == 0 & f2_ == 0)
Xwhithout0 = X[-c(which(f1_ == 0 & f2_ == 0)),]
dim(Xwhithout0)
Xwhithout0 = Xwhithout0[,-1]
params = adl.app(Xwhithout0, Z)
ad.val(params = params, Xwhithout0)
a = c(1,2,3,4,5,6)
a = a[-c(2,3,4)]


##### test scale pour régression logistique ##########
scaled.X <- scale(X)
spamErrorFunction(scaled.X, Z, logName, 20)
logtest = vector(length = 20)
for (i in 1:20) {
    logtest[i] = spamErrorFunction(scaled.X, Z, logName, 1)
}
mean(logtest)


######### K plus proches voisins #########
kppvErrorRates = vector(length = 20)
for (i in 1:20){
    sample_CV = separ2(X,Z)
    Kopt = kppv.tune(sample_CV$Xapp, sample_CV$zapp, sample_CV$Xval, sample_CV$zval, nppv = c(2*(1:6)-1))
    Kopt = min(Kopt)
    testPredictedClasses_CV = kppv.val(sample_CV$Xapp, sample_CV$zapp, Kopt, sample_CV$Xtst) # prédire les classes du jeu de données de test
    kppvErrorRates[i] = 1 - compute.sucess.rate(testPredictedClasses_CV, sample_CV$ztst)
    cat(i, "-")
}
mean(kppvErrorRates)


######### ACP avec scale et center #########
ACP = prcomp(X, center = T, scale = T)
plot(summary(ACP)$importance[3,], ylab = "Inertie expliquée cummulée", xlab = "Nombre de composantes principales considérées", pch=1)
plot(ACP$x[,1:2], col = c("black", "red")[Z])
ACP_X = ACP$x[,1:50]
spamErrorFunction(ACP_X, Z, qdaName, 20)
spamErrorFunction(ACP_X, Z, ldaName, 20)
spamErrorFunction(ACP_X, Z, nbaName, 20)
spamErrorFunction(ACP_X, Z, logName, 20)
spamErrorFunction(as.data.frame(ACP_X), Z, decisionTreeName, 20)
displayDecisionTree(as.data.frame(ACP_X),Z)
displayDecisionBorder(ACP_X, Z, model = logName)
nbComp = dim(ACP$x)[2]
adlTestComp = vector(length = nbComp)
hyperParamBestNbComposantes = list()
hyperParamBestNbComposantes[[qdaName]] = vector(length = nbComp)
hyperParamBestNbComposantes[[ldaName]] = vector(length = nbComp)
hyperParamBestNbComposantes[[nbaName]] = vector(length = nbComp)
hyperParamBestNbComposantes[[logName]] = vector(length = nbComp)
hyperParamBestNbComposantes[[decisionTreeName]] = vector(length = nbComp)
for (i in 2:10) {
    ACP_X_test = ACP$x[,1:i]
    #hyperParamBestNbComposantes[[qdaName]][i] = spamErrorFunction(ACP_X_test, Z, qdaName, 20)
    #hyperParamBestNbComposantes[[ldaName]][i] = spamErrorFunction(ACP_X_test, Z, ldaName, 20)
    #hyperParamBestNbComposantes[[nbaName]][i] = spamErrorFunction(ACP_X_test, Z, nbaName, 20)
    hyperParamBestNbComposantes[[logName]][i] = spamErrorFunction(ACP_X_test, Z, logName, 5)
    hyperParamBestNbComposantes[[decisionTreeName]][i] = spamErrorFunction(as.data.frame(ACP_X_test), Z, decisionTreeName, 20)
}
for (i in seq(12, 20, 2)) {
    ACP_X_test = ACP$x[,1:i]
    hyperParamBestNbComposantes[[logName]][i] = spamErrorFunction(ACP_X_test, Z, logName, 5)
    hyperParamBestNbComposantes[[decisionTreeName]][i] = spamErrorFunction(as.data.frame(ACP_X_test), Z, decisionTreeName, 20)
}
for (i in seq(25, nbComp, 5)) {
    ACP_X_test = ACP$x[,1:i]
    hyperParamBestNbComposantes[[logName]][i] = spamErrorFunction(ACP_X_test, Z, logName, 5)
    hyperParamBestNbComposantes[[decisionTreeName]][i] = spamErrorFunction(as.data.frame(ACP_X_test), Z, decisionTreeName, 20)
}
ACP_X_test = ACP$x
hyperParamBestNbComposantes[[logName]][nbComp] = spamErrorFunction(ACP_X_test, Z, logName, 5)
hyperParamBestNbComposantes[[decisionTreeName]][nbComp] = spamErrorFunction(as.data.frame(ACP_X_test), Z, decisionTreeName, 20)

hyperParamBestNbComposantes

plot(hyperParamBestNbComposantes$`Logistic Regression`, type = "p", col = "red", ylim = c(0.03, 0.15), ylab = "Taux d'erreur", xlab = "Nombre de composantes principales considérées")
points(hyperParamBestNbComposantes$`Decision Tree`, col = "blue")
xPlot = hyperParamBestNbComposantes$`Logistic Regression`
plot(replace(xPlot, xPlot==0, NaN), type = "p", col = "red", ylim = c(0.03, 0.15), ylab = "Taux d'erreur", xlab = "Nombre de composantes principales considérées", pch=1)
points(which.min(replace(xPlot, xPlot==0, NaN)), xPlot[which.min(replace(xPlot, xPlot==0, NaN))],col = "red", pch=8, cex=1)
xPlot = hyperParamBestNbComposantes$`Decision Tree`
points(replace(xPlot, xPlot==0, NaN), col = "blue")
points(which.min(replace(xPlot, xPlot==0, NaN)), xPlot[which.min(replace(xPlot, xPlot==0, NaN))],col = "blue", pch=8, cex=1)
legend(40,0.15, legend=c("Régression logistique", "Arbres de décision"), col=c("red", "blue"), pch=16, cex=.8)






################ Random forest #############

sample = separ1(X,Z)
valRandomForest = random.forest.app(sample$Xapp, sample$zapp, sample$Xtst, nbTrees = 100)
varImpPlot(fit)
prediction <- predict(fit, sample$Xtst)
(1 - compute.sucess.rate(prediction, sample$ztst))
getTree(valRandomForest, k=1, labelVar=TRUE)

