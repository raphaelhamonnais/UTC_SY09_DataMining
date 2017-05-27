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
fileNames_CE = c("data/Synth1-40.csv", "data/Synth1-100.csv", "data/Synth1-500.csv", "data/Synth1-1000.csv", "data/Synth2-1000.csv")
fileNames_CE = c("data/Breastcancer.csv", "data/Pima.csv")


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
estimatedMu_CE = list() # centres des classes <=> mean
estimatedProportions_CE = list()
estimatedSigma_CE = list() # matrices de covariances
for (i in 1:length(fileNames_CE)) {
    file = fileNames_CE[i]
    data = read.csv(file)
    beginDataIndex = 1
    endDatatIndex = 2
    zIndex = 3
    
    if (file == "data/Breastcancer.csv") {
        print("working with data/Breastcancer.csv")
        beginDataIndex = 1
        endDatatIndex = 9
        zIndex = 10
    }
    if (file == "data/Pima.csv") {
        print("working with data/Pima.csv")
        beginDataIndex = 1
        endDatatIndex = 7
        zIndex = 8
    }
    
    X = data[,beginDataIndex:endDatatIndex]
    Z = factor(data[,zIndex])
    g = length(levels(Z))
    p = ncol(X)
    cat("File : ", fileNames_CE[i])
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
    estimatedMu_CE[[file]] = currentFileMu
    estimatedProportions_CE[[file]] = currentFileProportion
    estimatedSigma_CE[[file]] = currentSigma
}

# affichage des paramètres estimés
for (file in fileNames_CE) {
    writeLines("-------------------------")
    writeLines(file)
    writeLines("-------------------------")
    writeLines("")
    
    writeLines("estimatedMu_CE")
    print(estimatedMu_CE[[file]])
    writeLines("")
    
    writeLines("estimatedProportions_CE")
    print(estimatedProportions_CE[[file]])
    writeLines("")
    
    writeLines("estimatedSigma_CE")
    print(estimatedSigma_CE[[file]])
    writeLines("--------------------------------------")
    writeLines("")
    writeLines("")
    writeLines("")
}


# Estimer le taux d'erreur
nbTests_CE = 20
alpha_CE = 0.05
detailledErrorRates_CE = list()
meanErrorRates_CE = list()
sdErrorRates_CE = list()
errorVariation_CE = list()
confidenceIntervals_CE = list()
for (i in 1:length(fileNames_CE)) {
    file = fileNames_CE[i]
    data = read.csv(file)
    beginDataIndex = 1
    endDatatIndex = 2
    zIndex = 3
    
    if (file == "data/Breastcancer.csv") {
        print("working with data/Breastcancer.csv")
        beginDataIndex = 1
        endDatatIndex = 9
        zIndex = 10
    }
    if (file == "data/Pima.csv") {
        print("working with data/Pima.csv")
        beginDataIndex = 1
        endDatatIndex = 7
        zIndex = 8
    }
    X = data[,beginDataIndex:endDatatIndex]
    Z = data[,zIndex]
    
    errorRates_CE = matrix(0, nrow = nbTests_CE, ncol = 2)
    colnames(errorRates_CE) = c("Error On App", "Error On Test")
    
    for (j in 1:nbTests_CE) {
        sample_CE = separ1(X,Z)
        mu = ceuc.app(sample_CE$Xapp, sample_CE$zapp) # calculer les paramètres du modèle, c'est à dire les centre de gravité des classes
        appPredictedClasses_CE = ceuc.val(mu, sample_CE$Xapp) # prédire les classes du jeu de données d'apprentissage
        testPredictedClasses_CE = ceuc.val(mu, sample_CE$Xtst) # prédire les classes du jeu de données de test
        appErrorRate_CE = 1 - compute.sucess.rate(appPredictedClasses_CE, sample_CE$zapp)
        testErrorRate_CE = 1 - compute.sucess.rate(testPredictedClasses_CE, sample_CE$ztst)
        errorRates_CE[j,1] = appErrorRate_CE
        errorRates_CE[j,2] = testErrorRate_CE
    }
    detailledErrorRates_CE[[file]] = errorRates_CE
    meanErrorRates_CE[[file]] = apply(errorRates_CE, 2, mean)
    sdErrorRates_CE[[file]] = apply(errorRates_CE, 2, sd)
    errorVariation_CE[[file]] = qt(1-alpha_CE/2, df=nbTests_CE-1) * sdErrorRates_CE[[file]] / sqrt(nbTests_CE)
    a[["left"]] = meanErrorRates_CE[[file]] - errorVariation_CE[[file]]
    a[["right"]] = meanErrorRates_CE[[file]] + errorVariation_CE[[file]]
    confidenceIntervals_CE[[file]] = a
}



# Affichage des taux d'erreur
for (file in fileNames_CE) {
    writeLines("-------------------------")
    writeLines(file)
    writeLines("-------------------------")
    print("nbTests_CE = ", nbTests_CE)
    
    writeLines("Estimation de l'erreur")
    print(meanErrorRates_CE[[file]])
    writeLines("")
    
    writeLines("Intervalles de confiance")
    nbCols = length(names(confidenceIntervals_CE[[file]]$left))
    for (i in 1:nbCols) {
        cat(
            "Intervalle pour",
            names(confidenceIntervals_CE[[file]]$left[i]),
            "[",
            confidenceIntervals_CE[[file]]$left[i],
            ";",
            confidenceIntervals_CE[[file]]$right[i],
            "]"
        )
        writeLines("")
    }
    
    writeLines("--------------------------------------")
    writeLines("")
    writeLines("")
}



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



