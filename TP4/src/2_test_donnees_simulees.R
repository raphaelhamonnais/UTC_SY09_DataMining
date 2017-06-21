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

syntFiles = c("data/Synth1-1000.csv","data/Synth2-1000.csv","data/Synth3-1000.csv")
pimaFile = c("data/Pima.csv")
brestcancerFile = c("data/bcw.csv")

qdaName = "Quadratic Discriminat Analysis"
ldaName = "Linear Discriminat Analysis"
nbaName = "Naive Bayes classifier"
logName = "Logistic Regression"
logQuadName = "Quadratic Logistic Regression"
decisionTreeName = "Decision Tree"
models = c(qdaName, ldaName, nbaName, logName, logQuadName, decisionTreeName)




################## Données synthétiques ################

for (m in models) {
    writeLines("--------")
    writeLines(m)
    writeLines("--------")
    meanErrorRates(syntFiles,m, nbTests = 20)
}

# Représentation graphique
plot2DimClasses(syntFiles[1])
legend(-5,5, legend=levels(factor(Z)), col=c("red", "black"), pch=16, cex=.8)
plot2DimClasses(syntFiles[2])
legend(-5,8, legend=levels(factor(Z)), col=c("red", "black"), pch=16, cex=.8)
plot2DimClasses(syntFiles[3])
legend(-5,8, legend=levels(factor(Z)), col=c("red", "black"), pch=16, cex=.8)

# frontières de décision
data = read.csv(syntFiles[1])
X = data[,1:2]
Z = data[,3]
displayDecisionBorder(X, Z, qdaName)
legend(-5,7, legend=levels(factor(Z)), col=c("red", "black"), pch=16, cex=.8)
displayDecisionBorder(X, Z, ldaName)
displayDecisionBorder(X, Z, nbaName)
displayDecisionBorder(X, Z, logName)
displayDecisionBorder(X, Z, logQuadName)
displayDecisionBorder(X, Z, decisionTreeName)
displayDecisionTree(X, Z, 104)










################## Données Pima ################

for (m in models) {
    writeLines("--------")
    writeLines(m)
    writeLines("--------")
    meanErrorRates(pimaFile,m, 100)
}
plotAfterACPClasses(pimaFile, scaleEnabled = F, centerEnabled = F)
data = read.csv(pimaFile)
sqrt(diag(var(data)))
zIndex = dim(data)[2]
X = data[,1:zIndex-1]
Z = data[,zIndex]
displayDecisionTree(X, Z, 104)

#ACP
acp = prcomp(X, center = F, scale = F)
plot(acp$x[,1:2], col=Z)
inert = (acp$sdev)^2
inertExplique = inert / sum(inert)
inertExpliqueCumul = inertExplique
for (i in 1:length(inertExpliqueCumul))
    inertExpliqueCumul[i] = sum(inertExplique[1:i])










################## Données breast cancer ################
plotAfterACPClasses(brestcancerFile)
for (m in models) {
    writeLines("--------")
    writeLines(m)
    writeLines("--------")
    meanErrorRates(brestcancerFile,m, 100)
}

data = read.csv(brestcancerFile)
zIndex = dim(data)[2]
X = data[,1:zIndex-1]
Z = data[,zIndex]
displayDecisionTree(X, Z, 106)


















