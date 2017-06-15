library("MASS")

source("src/fonctions/separ1.R")
source("src/fonctions/mvdnorm.r")
source("src/fonctions/anadisc.R")
source("src/fonctions/logistic.R")
source("src/tree.R")
source("src/fonctions/functions.util.R")

# Calculation le taux d'erreur de test moyen
meanErrorRates <- function(fileNames, model_global, model) {
  detailledErrorRates = list()
  meanErrorRates = list()
  intr = T
  epsi = 1e-5
  
  for(i in 1:length(fileNames)) {
    file = fileNames[i]
    data = read.csv(file, header = T)
    zIndex = 3
    nbTests = 20
    
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
      if(model_global == "Discriminat Analysis") {
        if (model == "Quadratic Discriminat Analysis") {
          params = adq.app(sample$Xapp, sample$zapp) # calculer les paramètres du modèle
        } 
        else if (model == "Linear Discriminat Analysis") {
          params = adl.app(sample$Xapp, sample$zapp)
        }
        else if (model == "Naive Bayes classifier") {
          params = nba.app(sample$Xapp, sample$zapp)
        }
        testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
        testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$ztst, sample$ztst)
        modelTested = model
      }
      else if (model_global == "Logistic Regression") {
        if (model == "Logistic Regression") {
          params = log.app(sample$Xapp, sample$zapp, intr, epsi)
        }
        else if (model == "Quadratic Logistic Regression") {
          if(file == "data/bcw.csv") {
            cat("We don't use the method ",model," when the data is ",file,".\n")
            break
          }
          else {
            params = log.app(sample$Xapp, sample$zapp, intr, epsi)
          }
        }
        testPredictedClasses = log.val(params$beta,sample$Xtst)
        testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
        modelTested = model
      }
      else if (model_global == "Decision Tree") {
        testPredictedClasses <- tree.app(sample$Xapp,sample$zapp,sample$Xtst,sample$ztst)
        testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$ztst, sample$ztst)
        modelTested = model
      }
      errorRates[j,1] = testErrorRate
    }
    detailledErrorRates[[file]] = errorRates   
    meanErrorRates[[file]] = apply(errorRates, 2, mean)
  }
  
  # Affichage des taux d'erreur
  writeLines("-------------------------")
  cat("Method:", modelTested,"\n")
  writeLines("-------------------------")
  for (file in fileNames) {
      error = round(meanErrorRates[[file]], 3)
      cat(file, error, "\n")
  }
}



meanErrorRates <- function(fileNames, model) {
    detailledErrorRates = list()
    meanErrorRates = list()
    intr = T
    epsi = 1e-5
    
    for(i in 1:length(fileNames)) {
        file = fileNames[i]
        data = read.csv(file, header = T)
        zIndex = dim(data)[2]
        nbTests = 20
        
        if (file == "data/Pima.csv") {
            print("working with data/Pima.csv")
            nbTests = 100
        }
        if (file == "data/bcw.csv") {
            print("working with data/bcw.csv")
            nbTests = 100
        }
        if (file == "data/spam2.csv") {
            print("working with data/spam2.csv")
        }
        
        X = data[,1:zIndex-1]
        Z = data[,zIndex]
        
        errorRates = matrix(0, nrow = nbTests, ncol = 1)
        colnames(errorRates) = c("Error On Test")
        
        for (j in 1:nbTests) {
            sample = separ1(X,Z)
            if (model == "Quadratic Discriminat Analysis") {
                params = adq.app(sample$Xapp, sample$zapp) # calculer les paramètres du modèle
                testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
                testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$ztst, sample$ztst)
            } 
            else if (model == "Linear Discriminat Analysis") {
                params = adl.app(sample$Xapp, sample$zapp)
                testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
                testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$ztst, sample$ztst)
            }
            else if (model == "Naive Bayes classifier") {
                params = nba.app(sample$Xapp, sample$zapp)
                testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
                testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$ztst, sample$ztst)
            }
            if (model == "Logistic Regression") {
                params = log.app(sample$Xapp, sample$zapp, intr, epsi)
                testPredictedClasses = log.val(params$beta,sample$Xtst)
                testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
            }
            else if (model == "Quadratic Logistic Regression") {
                if(file == "data/bcw.csv") {
                    cat("We don't use the method ",model," when the data is ",file,".\n")
                    break
                }
                else {
                    params = log.app(sample$Xapp, sample$zapp, intr, epsi)
                    testPredictedClasses = log.val(params$beta,sample$Xtst)
                    testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
                }
            }
            else if (model == "Decision Tree") {
                testPredictedClasses <- tree.app(sample$Xapp,sample$zapp,sample$Xtst)
                testErrorRate = 1 - compute.sucess.rate(testPredictedClasses, sample$ztst)
            }
            errorRates[j,1] = testErrorRate
        }
        detailledErrorRates[[file]] = errorRates   
        meanErrorRates[[file]] = apply(errorRates, 2, mean)
    }
    
    # Affichage des taux d'erreur
    for (file in fileNames) {
        writeLines("-------------------------")
        writeLines(file)
        writeLines("-------------------------")
        print(meanErrorRates[[file]])
        #error = round(meanErrorRates[[file]], 4) * 100
        #cat(model, error, "\n")
    }
}


spam = read.csv("data/spam.csv", header = T)
zIndex = dim(spam)[2] # nombre de variable
# summary(spam)
# head(spam)
X = spam[,2:(zIndex-1)]
Z = spam[,zIndex]

# PCA pour des donnees
spam_pca = prcomp(X, center = FALSE,  scale. = FALSE)
summary(spam_pca)


# On prend le 90% de données comme les composants principales
spam_pca_new = spam_pca$x[,1:2]
# head(spam_pca_new)
spam_new = cbind(spam_pca_new,Z)
# head(spam_new)
# créer un nouveau fichier csv pour les données spam.csv
write.csv(spam_new, file = "data/spam2.csv", row.names = FALSE)


fileNames = c("data/Synth1-1000.csv","data/Synth2-1000.csv","data/Synth3-1000.csv")
fileNames = c("data/Pima.csv")
fileNames = c("data/bcw.csv")
# fileNames = c("data/spam.csv")
fileNames = c("data/spam2.csv")
model = c("Quadratic Discriminat Analysis", 
          "Linear Discriminat Analysis", 
          "Naive Bayes classifier", 
          "Logistic Regression", 
          "Quadratic Logistic Regression",
          "Decision Tree")


for (m in model) {
    #print(m)
    meanErrorRates(fileNames,m)
}

















