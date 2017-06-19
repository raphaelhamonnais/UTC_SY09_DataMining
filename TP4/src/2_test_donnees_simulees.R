library("MASS")

source("src/fonctions/separ1.R")
source("src/fonctions/mvdnorm.r")
source("src/fonctions/anadisc.R")
source("src/fonctions/logistic.R")
source("src/fonctions/tree.R")
source("src/fonctions/functions.util.R")
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



# Calculation le taux d'erreur de test moyen
meanErrorRates <- function(fileNames, model, nbTests=20) {
    detailledErrorRates = list()
    meanErrorRates = list()
    intr = T
    epsi = 1e-5
    
    for(i in 1:length(fileNames)) {
        file = fileNames[i]
        data = read.csv(file, header = T)
        zIndex = 3
        
        if (file == "data/Pima.csv") {
            print("working with data/Pima.csv")
            zIndex = 8
            nbTests = 100
        }
        if (file == "data/bcw.csv") {
            print("working with data/bcw.csv")
            zIndex = 10
            nbTests = 100
        }
        if (file == "data/spam.csv") {
            print("working with data/spam.csv")
            zIndex = 58
        }
        
        X = data[,1:zIndex-1]
        Z = data[,zIndex]
        
        errorRates = matrix(0, nrow = nbTests, ncol = 1)
        colnames(errorRates) = c("Error On Test")
        
        for (j in 1:nbTests) {
            sample = separ1(X,Z)
            if (model == qdaName) {
                params = adq.app(sample$Xapp, sample$zapp) # calculer les paramètres du modèle
                testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
            } 
            else if (model == ldaName) {
                params = adl.app(sample$Xapp, sample$zapp)
                testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
            }
            else if (model == nbaName) {
                params = nba.app(sample$Xapp, sample$zapp)
                testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
            }
            else if (model == logName) {
                params = log.app(sample$Xapp, sample$zapp, intr, epsi)
                testPredictedClasses = log.val(params$beta,sample$Xtst)
            }
            else if (model == logQuadName) {
                if(file == brestcancerFile) {
                    cat("We don't use the method ",model," when the data is ",file,".\n")
                    break
                }
                else {
                    params = log_quad.app(sample$Xapp, sample$zapp, intr, epsi)
                    testPredictedClasses = log_quad.val(params$beta, sample$Xtst)
                }
            }
            else if (model == decisionTreeName) {
                testPredictedClasses <- tree.app(sample$Xapp,sample$zapp,sample$Xtst)
            }
            else {
                cat("Error, no model found for ", model, "\n")
            }
            errorRates[j,1] = 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
        }
        detailledErrorRates[[file]] = errorRates   
        meanErrorRates[[file]] = apply(errorRates, 2, mean)
    }
    
    # Affichage des taux d'erreur
    for (file in fileNames) {
        cat(file, round(meanErrorRates[[file]], 4) * 100, "\n")
    }
}
displayDecisionBorder <- function(X, Z, model) {
    sample = separ1(X, Z)
    if (model == qdaName)
        params = adq.app(sample$Xapp, sample$zapp)
    else if (model == ldaName)
        params = adl.app(sample$Xapp, sample$zapp)
    else if (model == nbaName)
        params = nba.app(sample$Xapp, sample$zapp)
    else if (model == logName)
        params = log.app(sample$Xapp, sample$zapp, intr = T, epsi = 1e-5)
    else if (model == logQuadName)
        params = log_quad.app(sample$Xapp, sample$zapp, intr = T, epsi = 1e-5)
    else if (model == decisionTreeName)
        testPredictedClasses <- tree.app(sample$Xapp,sample$zapp,sample$Xtst)
    else
        cat("Error, no model found for ", model, "\n")
    
    
    if (model == qdaName | model == ldaName | model == nbaName)
        prob.ad(params, sample$Xtst, sample$ztst, seq(0, 1, 0.25))
    else if (model == logName)
        prob.log(params$beta, sample$Xtst, sample$ztst, seq(0,1,0.25))
    else if (model == logQuadName)
        prob.log2(params$beta, sample$Xtst, sample$ztst, seq(0,1,0.25))
    else if (model == decisionTreeName) {
        plot(sample$Xtst, col = sample$ztst)
        partition.tree(pred$pruneTree, add = T)
    }
    else
        cat("Error, no model found for ", model, "\n")
}
    


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
data = read.csv(syntFiles[3])
X = data[,1:2]
Z = data[,3]
displayDecisionBorder(X, Z, qdaName)
legend(-5,7, legend=levels(factor(Z)), col=c("red", "black"), pch=16, cex=.8)
displayDecisionBorder(X, Z, ldaName)
displayDecisionBorder(X, Z, nbaName)
displayDecisionBorder(X, Z, logName)
displayDecisionBorder(X, Z, logQuadName)
displayDecisionBorder(X, Z, decisionTreeName)








################## Données Pima ################
plotAfterACPClasses(pimaFile)
for (m in models) {
    writeLines("--------")
    writeLines(m)
    writeLines("--------")
    meanErrorRates(pimaFile,m, 100)
}











################## Données breast cancer ################
plotAfterACPClasses(brestcancerFile)
for (m in models) {
    writeLines("--------")
    writeLines(m)
    writeLines("--------")
    meanErrorRates(brestcancerFile,m, 100)
}




















