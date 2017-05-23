library(mclust)
source("src/fonctions-tp3/distXY.R")
source("src/fonctions-tp3/front.ceuc.R")
source("src/fonctions-tp3/front.kppv.R")
source("src/fonctions-tp3/separ1.R")
source("src/fonctions-tp3/separ2.R")
source("src/Fonctions_Euclidien.R")
source("src/Fonctions_Utilities.R")

appData = read.csv("data/Synth1-40.csv")
Xapp = appData[,1:2]
zapp = factor(appData[,3])

testData = read.csv("data/Synth1-40.csv")
Xtst = testData[,1:2]
ztst = factor(testData[,3])


############## 1.1.3 Test des fonctions ##############
mu <- ceuc.app(Xapp, zapp)
front.ceuc(mu, Xtst, ztst, 500)



############## 1.2 Évaluation des performances ##############
fileNames = c("data/Synth1-40.csv", "data/Synth1-100.csv", "data/Synth1-500.csv", "data/Synth1-1000.csv", "data/Synth2-1000.csv")

### 1 - Classifieur euclidien - Estimer les paramètres
# Pour chacun des jeux de données, estimer les paramètres μk et Σk des distributions conditionnelles, ainsi que les proportions πk des classes.
# πk = proportion
# μk = les centre de gravité des classes => means
# Σk = matrice de covariance entre les variables
#
# on estime donc les paramètres du modèle (centres des classes, matrices de covariance, proportions) 
# puis on regarde si les hypothèses sont (raisonnablement) vérifiées (raisonnablement : ex les 
# matrices de covariance peuvent ne pas être exactement diagonales mais presque — ie termes non 
# diagonaux négligeables).
#
# L'idée est donc d'interpréter les résultats obtenus à la lumière de ce qu'on sait sur les méthodes utilisées
#   - pour ce qui est du travail avec le classifieur euclidien, il marchera bien si
#       - on a bien des proportions qui se rapprochent de 1/g (donc ici 0.5 car g=2)
#       - que les Σk sont égales entre-elles (même dispersion)
#       - que la dispersion est sphérique, c’est à dire que les Σk sont des matrices diagonales avec
#           des termes diagonaux nul ou négligeables

#
#
estimatedMu = list() # centres des classes <=> mean
estimatedProportions = list()
estimatedSigma = list() # matrices de covariances
for (i in 1:length(fileNames)) {
    file = fileNames[i]
    data = read.csv(file)
    X = data[,1:2]
    Z = factor(data[,3])
    g = length(levels(Z))
    p = ncol(X)
    cat("File : ", fileNames[i])
    writeLines("")
    
    currentFileMu = matrix(nrow = g, ncol = p)
    rownames(currentFileMu) = levels(Z) # mettre les noms des classes sur les lignes
    colnames(currentFileMu) = colnames(X)
    
    currentFileProportion = matrix(nrow = g, ncol = 1)
    rownames(currentFileProportion) = levels(Z) # mettre les noms des classes sur les lignes
    
    currentSigma = list()
    for (level in levels(Z)) {
        classData = X[Z == level,]
        currentFileMu[level,] = apply(classData, 2, mean) # calculer la moyenne pour chaque classe
        currentFileProportion[level,] = nrow(classData) / nrow(X)
        currentSigma[[level]] = var(classData)
    }
    estimatedMu[[file]] = currentFileMu
    estimatedProportions[[file]] = currentFileProportion
    estimatedSigma[[file]] = currentSigma
}
estimatedMu
estimatedProportions
estimatedSigma


# Estimer le taux d'erreur
nbTests = 20
alpha = 0.05
detailledErrorRates = list()
meanErrorRates = list()
sdErrorRates = list()
errorVariation = list()
confidenceIntervals = list()
for (i in 1:length(fileNames)) {
    file = fileNames[i]
    data = read.csv(file)
    X = data[,1:2]
    Z = data[,3]
    
    errorRates = matrix(0, nrow = nbTests, ncol = 2)
    colnames(errorRates) = c("Error On App", "Error On Test")
    
    for (j in 1:nbTests) {
        samples = separ1(X,Z)
        mu = ceuc.app(samples$Xapp, samples$zapp) # calculer les paramètres du modèle, c'est à dire les centre de gravité des classes
        appPredictedClasses = ceuc.val(mu, samples$Xapp) # prédire les classes du jeu de données d'apprentissage
        testPredictedClasses = ceuc.val(mu, samples$Xtst) # prédire les classes du jeu de données de test
        appErrorRate = 1 - compute.sucess.rate(appPredictedClasses, samples$zapp)
        testErrorRate = 1 - compute.sucess.rate(testPredictedClasses, samples$ztst)
        errorRates[j,1] = appErrorRate
        errorRates[j,2] = testErrorRate
    }
    detailledErrorRates[[file]] = errorRates
    meanErrorRates[[file]] = apply(errorRates, 2, mean)
    sdErrorRates[[file]] = apply(errorRates, 2, sd)
    errorVariation[[file]] = qt(1-alpha/2, df=nbTests-1) * sdErrorRates[[file]] / sqrt(nbTests)
    a[["left"]] = meanErrorRates[[file]] - errorVariation[[file]]
    a[["right"]] = meanErrorRates[[file]] + errorVariation[[file]]
    confidenceIntervals[[file]] = a
}

confidenceIntervals$`data/Synth1-40.csv`$left
confidenceIntervals$`data/Synth1-40.csv`$right


detailledErrorRates
meanErrorRates
sdErrorRates$`data/Synth1-40.csv`
errorVariation$`data/Synth1-40.csv`



############# INTERVALLES DE CONFIANCE #####################
# par définition, un moyenne suit une loi gausienne, on peut donc obtenir l'intervalle de confiance via
#   20 erreurs suivant la meme loi et tirées selon une loi qu'on ne connait pas mais on considère que ces 20 tirages sont indépendants car séparations différentes avec la fonction separ1
#   "assuming that the original random variable is normally distributed, and the samples are independent"
#       Donc tirages indépendants
#       moyenne X-Barre de chaque tirage suit une loi gaussienne
#       Vecteur de X-Barre contient 20 erreurs qui suivent une loi gausienne par le TCL
#       Donc on a l'intervalle de confiance avec 
#           Après centrage et réduction de la moyenne empirique, on obtient : sqrt(n)(mean(x)-m)/sd(x) ~ N(0,1)
#           Avec variance inconnue on a sqrt(n)(mean(x)-m)/sd(x) ~ St(n-1) loi de student à n-1 degrés de liberté



