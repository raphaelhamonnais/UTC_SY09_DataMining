library(mclust)
source("src/fonctions-tp3/distXY.R")
source("src/fonctions-tp3/front.ceuc.R")
source("src/fonctions-tp3/front.kppv.R")
source("src/Fonctions_Utilities.R")

kppv.val = function(Xapp, zapp, K, Xtst) {
    zapp = factor(zapp)
    testSize = nrow(Xtst)
    ztst = vector(length = testSize)
    
    # calculer les distances
    distances = distXY(Xapp, Xtst) # les distances de Xtst à Xapp sont en colonnes
    orderedDistancesIndexes = apply(distances, 2, order) # contient les indexs des distances triées dans l'ordre croissants
    
    # trouver les K plus proches voisins pour chaque individu de la population de test
    for (i in 1:testSize) {
        neighborsIndexes = orderedDistancesIndexes[1:K,i] # récupérer les K premiers index, c'est à dire les K plus proches voisins
        #cat(neighborsIndexes, "   ")
        neighborsClasses = zapp[neighborsIndexes] # récupérer les classes des K plus proches voisins
        #cat(neighborsClasses, "   ")
        neighborsClassesContingency = table(neighborsClasses) # compter le nombre de représentants par classe
        #cat(neighborsClassesContingency, "   ")
        mostRepresentedClass = names(which.max(neighborsClassesContingency)) # récupérer le nom de la classe la plus représenté
        #cat(mostRepresentedClass, "   ")
        ztst[i] = mostRepresentedClass
    }
    return(factor(ztst))
}

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv, skipOneNeighbor = TRUE, useRandIndexes = FALSE) {
    nbKToTest = length(nppv)
    maxSuccessRate = 0
    optimumForK = c()
    
    for (i in 1:nbKToTest) {
        K = nppv[i]
        if (skipOneNeighbor) if (K == 1) next
        zvalPredicted = kppv.val(Xapp, zapp, K, Xval)
        
        if (useRandIndexes)
            successRate = adjustedRandIndex(zval, zvalPredicted)
        else
            successRate = compute.sucess.rate(predictedClasses = zvalPredicted, actualClasses = zval)
        
        if (successRate >= maxSuccessRate) {
            if (successRate == maxSuccessRate) {
                optimumForK = c(optimumForK, K) # ajouter une autre valeur optimale possible de K
            }
            else { # "successRate" supérieur à "maxSuccessRate"
                optimumForK = c(K) # remplacer la valeur optimale de K
                maxSuccessRate = successRate # mettre à jour le "maxSuccessRate"
            }
        }
    }
    return(optimumForK)
}





################# TEST DE LA FONCTION kppv.val ######################

appData = read.csv("data/Synth1-1000.csv")
Xapp = appData[,1:2]
zapp = factor(appData[,3])

valData = read.csv("data/Synth1-500.csv")
Xval = valData[,1:2]
zval = factor(valData[,3])

affData = read.csv("data/Synth1-40.csv")
Xaff = affData[,1:2]
zaff = factor(affData[,3])

nppv = c(2*(1:6)-1)

Kopt = kppv.tune(Xapp, zapp, Xval, zval, nppv)
Kopt
front.kppv(Xaff, zaff, Kopt[1], 500)

################ TEST DE LA FONCTION kppv.tune ######################


appData = read.csv("data/Synth1-1000.csv")
Xapp = appData[,1:2]
zapp = factor(appData[,3])

valData = read.csv("data/Synth1-500.csv")
Xval = valData[,1:2]
zval = factor(valData[,3])



Kopt = kppv.tune(Xapp, zapp, Xval, zval, nppv)
Kopt
