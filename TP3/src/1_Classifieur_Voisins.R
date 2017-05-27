library(mclust)
source("src/fonctions-tp3/distXY.R")
source("src/fonctions-tp3/front.ceuc.R")
source("src/fonctions-tp3/front.kppv.R")
source("src/Fonctions_Voisins.R")
source("src/Fonctions_Utilities.R")



appData = read.csv("data/Synth1-1000.csv")
Xapp = appData[,1:2]
zapp = factor(appData[,3])

valData = read.csv("data/Synth1-500.csv")
Xval = valData[,1:2]
zval = factor(valData[,3])

testData = read.csv("data/Synth1-40.csv")
Xtst = testData[,1:2]
ztst = factor(testData[,3])


############## 1.1.3 Test des fonctions ##############
nppv = c(2*(1:6)-1)
Kopt <- kppv.tune(Xapp, zapp, Xval, zval, nppv)
Kopt
front.kppv(Xtst, ztst, Kopt, 500)



############## 1.2 Évaluation des performances ##############

fileNames_CV = c("data/Synth1-40.csv", "data/Synth1-100.csv", "data/Synth1-500.csv", "data/Synth1-1000.csv", "data/Synth2-1000.csv")

############# 1.2.1 #############
# Question 3. Effectuer une séparation aléatoire de l’ensemble de données en un ensemble 
# d’apprentissage et un ensemble de test. Déterminer le nombre optimal de voisins à l’aide 
# de la fonction kppv.tune, en utilisant l’ensemble d’apprentissage comme ensemble de validation. 
# Quel est le nombre optimal de voisins déterminé ? Pourquoi ?

data = read.csv("data/Synth1-1000.csv")
X = data[,1:2]
Z = data[,3]
sample_CV = separ1(X,Z)
Kopt = kppv.tune(sample_CV$Xapp, sample_CV$zapp, sample_CV$Xapp, sample_CV$zapp, nppv = c(2*(1:6)-1), skipOneNeighbor = FALSE, useRandIndexes = F)
Kopt
# K optimal = 1 car on fait du surapprentissage : le plus proche voisin d'un point étant lui-meme, on ne pourra pas avoir de meilleur résultat (ie 0% d'erreur) qu'avec le choix de K=1

    
# Comme pour le classifieur euclidien, écrire un script qui effectue N = 20 séparations
# aléa- toires de chaque jeu de données en ensembles d’apprentissage, de validation, 
# et de test ; et qui pour chacune détermine le nombre optimal de voisins à partir 
# d’un ensemble de vali- dation spécifique, puis calcule (et stocke) le taux d’erreur 
# sur l’ensemble d’apprentissage et sur l’ensemble de test

# Estimer le taux d'erreur
nbTests_CV = 20
alpha_CV = 0.05
detailledErrorRates_CV = list()
meanErrorRates_CV = list()
sdErrorRates_CV = list()
errorVariation_CV = list()
confidenceIntervals_CV = list()
for (i in 1:length(fileNames_CV)) {
    file = fileNames_CV[i]
    data = read.csv(file)
    X = data[,1:2]
    Z = data[,3]
    
    errorRates_CV = matrix(0, nrow = 20, ncol = 2)
    colnames(errorRates_CV) = c("Error On App", "Error On Test")
    
    for (j in 1:nbTests_CV) {
        sample_CV = separ2(X,Z)
        Kopt = kppv.tune(sample_CV$Xapp, sample_CV$zapp, sample_CV$Xval, sample_CV$zval, nppv = c(2*(1:6)-1))
        Kopt = min(Kopt)
        appPredictedClasses_CV = kppv.val(sample_CV$Xapp, sample_CV$zapp, Kopt, sample_CV$Xapp) # prédire les classes du jeu de données d'apprentissage
        testPredictedClasses_CV = kppv.val(sample_CV$Xapp, sample_CV$zapp, Kopt, sample_CV$Xtst) # prédire les classes du jeu de données de test
        appErrorRate_CV = 1 - compute.sucess.rate(appPredictedClasses_CV, sample_CV$zapp)
        testErrorRate_CV = 1 - compute.sucess.rate(testPredictedClasses_CV, sample_CV$ztst)
        errorRates_CV[j,1] = appErrorRate_CV
        errorRates_CV[j,2] = testErrorRate_CV
    }
    detailledErrorRates_CV[[file]] = errorRates_CV
    meanErrorRates_CV[[file]] = apply(errorRates_CV, 2, mean)
    sdErrorRates_CV[[file]] = apply(errorRates_CV, 2, sd)
    errorVariation_CV[[file]] = qt(1-alpha_CV/2, df=nbTests_CV-1) * sdErrorRates_CV[[file]] / sqrt(nbTests_CV)
    a[["left"]] = meanErrorRates_CV[[file]] - errorVariation_CV[[file]]
    a[["right"]] = meanErrorRates_CV[[file]] + errorVariation_CV[[file]]
    confidenceIntervals_CV[[file]] = a
}

meanErrorRates_CV
meanErrorRates_CV$`data/Synth1-40.csv`
