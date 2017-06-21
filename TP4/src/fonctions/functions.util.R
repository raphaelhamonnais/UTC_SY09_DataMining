library("plyr")
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

compute.sucess.rate = function(predictedClasses, actualClasses) {
    zvalPredictedKnowingZvalContingency = table(predictedClasses, actualClasses)
    correctPredictions = sum(diag(zvalPredictedKnowingZvalContingency))
    totalPredictions = sum(zvalPredictedKnowingZvalContingency)
    successRate = correctPredictions / totalPredictions
}


plot2DimClasses <- function(file) {
    data <- read.csv(file)
    zIndex = dim(data)[2]
    X <- data[,1:zIndex-1]
    Z <- data[,zIndex]
    class_colors = mapvalues(Z, from = c("1", "2"), to = c("red", "black"))
    class_colors = as.character(class_colors)
    #plot(X, col=c("red", "black")[Z])
    plot(X, col=class_colors)
    params = adq.app(X,Z)
    print(file)
    print("Classe 1")
    print("params$pi[[1]]")
    print(round(params$pi[[1]], 2))
    print("params$mu[[1]]")
    print(round(params$mu[[1]], 2))
    print("params$sigma[[1]]")
    print(round(params$sigma[[1]], 2))
    
    print("Classe 2")
    print("params$pi[[2]]")
    print(round(params$pi[[2]], 2))
    print("params$mu[[2]]")
    print(round(params$mu[[2]], 2))
    print("params$sigma[[2]]")
    print(round(params$sigma[[2]], 2))
}
plotAfterACPClasses <- function(file, centerEnabled = T, scaleEnabled = T) {
    data <- read.csv(file)
    zIndex = dim(data)[2]
    X <- data[,1:zIndex-1]
    Z <- data[,zIndex]
    acp = prcomp(X, center = centerEnabled, scale = scaleEnabled)
    plot(acp$x[,1:2], col=c("red", "black")[Z])
    return(acp)
}



spamErrorFunction = function(X,Z,model, nbTests = 20, file = "spam.csv", printBeforeError = F) {
    intr = T
    epsi = 1e-5
    detailledErrorRates = list()
    meanErrorRates = list()
    mean_adl_f1_f2_nul = list()
    
    errorRates = matrix(0, nrow = nbTests, ncol = 1)
    colnames(errorRates) = c("Error On Test")
    
    adl_f1_f2_nul = matrix(0, nrow = nbTests, ncol = 1)
    for (j in 1:nbTests) {
        sample = separ1(X,Z)
        if (model == qdaName) {
            params = adq.app(sample$Xapp, sample$zapp) # calculer les paramètres du modèle
            testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
        } 
        else if (model == ldaName) {
            params = adl.app(sample$Xapp, sample$zapp)
            testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
            adl_f1_f2_nul[j,1] = testPredictedClasses$nbRowsWithBothDensityEqualsToZero
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
        else if (model == kppvName) {
            sample_CV = separ2(X,Z)
            Kopt = kppv.tune(sample_CV$Xapp, sample_CV$zapp, sample_CV$Xval, sample_CV$zval, nppv = c(2*(1:6)-1))
            Kopt = min(Kopt)
            predictionKPPV = kppv.val(sample_CV$Xapp, sample_CV$zapp, Kopt, sample_CV$Xtst) # prédire les classes du jeu de données de test
            testPredictedClasses = NULL
            testPredictedClasses$pred = 1 - compute.sucess.rate(predictionKPPV, sample$ztst)
        }
        else {
            cat("Error, no model found for ", model, "\n")
        }
        
        errorRates[j,1] = 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
        
        if (printBeforeError)
            print(errorRates[j,1])
    }
    detailledErrorRates = errorRates
    meanErrorRates = apply(errorRates, 2, mean)
    cat(model, round(meanErrorRates, 4), "\n")
    if (model == ldaName) {
        mean_adl_f1_f2_nul = apply(adl_f1_f2_nul, 2, mean)
        cat("ADL : observations with f1 = f2 = 0", round(mean_adl_f1_f2_nul, 4) * 100, "% \n")
    }
    
    return(round(meanErrorRates, 4))
}



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
            zIndex = 8
            nbTests = 100
        }
        if (file == "data/bcw.csv") {
            zIndex = 10
            nbTests = 100
        }
        if (file == "data/spam.csv") {
            zIndex = 58
        }
        
        X = data[,1:zIndex-1]
        Z = data[,zIndex]
        if (file == "data/spam.csv") {
            X = X[,-1]
        }
        
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
        plot(sample$Xtst, col = c("red", "black")[sample$ztst])
        partition.tree(testPredictedClasses$treeForDecisionBorder, add = T)
    }
    else
        cat("Error, no model found for ", model, "\n")
}
displayDecisionTree <- function(X, Z, mode = 104) {
    sample = separ1(X, Z)
    testPredictedClasses <- tree.app(sample$Xapp,sample$zapp,sample$Xtst)
    errorPrint = 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
    prp(testPredictedClasses$optimalTree,extra=mode, legend.y = "errorPrint")
    cat("Error rate = ", 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst), "\n")
}
