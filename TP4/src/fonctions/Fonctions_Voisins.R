library(mclust)
source("src/fonctions/distXY.R")

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
