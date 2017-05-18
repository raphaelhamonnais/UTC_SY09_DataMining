library(mclust)
source("src/fonctions-tp3/distXY.R")
source("src/fonctions-tp3/front.ceuc.R")
source("src/fonctions-tp3/front.kppv.R")
maxN <- function(x, N=2){
    len <- length(x)
    if(N>len){
        warning('N greater than length(x).  Setting N=length(x)')
        N <- length(x)
    }
    sort(x,partial=len-N+1)[len-N+1]
}
minN <- function(x, N=2){
    len <- length(x)
    if(N>len){
        warning('N greater than length(x).  Setting N=length(x)')
        N <- length(x)
    }
    sort(x,partial=len-(len-N))[N]
}

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
        #writeLines("")
        #writeLines("")
    }
    return(factor(ztst))
}

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv, skipOneNeighbor = TRUE, useRandIndexes = FALSE) {
    nbKToTest = length(nppv)
    maxSuccessRate = 0
    optimumForK = c() # liste vide au début
    
    for (i in 1:nbKToTest) {
        K = nppv[i]
        if (skipOneNeighbor) if (K == 1) next
        cat("K = ", K, "   ")
        zvalPredicted = kppv.val(Xapp, zapp, K, Xval)
        
        if (useRandIndexes) {
            successRate = adjustedRandIndex(zval, zvalPredicted)
        }
        else {
            zvalPredictedKnowingZvalContingency = table(zvalPredicted, zval)
            correctPredictions = sum(diag(zvalPredictedKnowingZvalContingency))
            totalPredictions = sum(zvalPredictedKnowingZvalContingency)
            successRate = correctPredictions / totalPredictions
        }
        cat("successRate = ", successRate, "   ")
        
        if (successRate >= maxSuccessRate) {
            if (successRate == maxSuccessRate) {
                optimumForK = c(optimumForK, K) # ajouter une autre valeur optimale possible de K
            }
            else { # "successRate" supérieur à "maxSuccessRate"
                optimumForK = c(K) # remplacer la valeur optimale de K
                maxSuccessRate = successRate # mettre à jour le "maxSuccessRate"
            }
            cat("optimumForK = ", optimumForK, "   ")
        }
        writeLines("")
    }
    return(optimumForK)
}





################# TEST DE LA FONCTION kppv.val ######################

#appData = read.csv("data/Synth1-8.csv")
#Xapp = appData[,1:2]
#zapp = factor(appData[,3])

#valData = read.csv("data/Synth1-8-bis.csv")
#Xval = valData[,1:2]
#zval = factor(valData[,3])

#testData = read.csv("data/Synth1-8-bis.csv")
#Xtst = testData[,1:2]
#ztst = factor(testData[,3])

#kppv.val(Xapp, zapp, 3, Xtst)

################# TEST DE LA FONCTION kppv.tune ######################


#appData = read.csv("data/Synth1-1000.csv")
#Xapp = appData[,1:2]
#zapp = factor(appData[,3])

#valData = read.csv("data/Synth1-500.csv")
#Xval = valData[,1:2]
#zval = factor(valData[,3])

#nppv = c(2*(1:6)-1)

#Kopt = kppv.tune(Xapp, zapp, Xval, zval, nppv)
#Kopt
