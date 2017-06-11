library("MASS")

source("src/fonctions/separ1.R")
source("src/fonctions/mvdnorm.r")
source("src/fonctions/anadisc.R")
source("src/fonctions/logistic.R")
source("src/tree.R")
source("src/fonctions/functions.util.R")

# Des données utilisées 
fileNames = c("data/Synth1-1000.csv","data/Synth2-1000.csv","data/Synth3-1000.csv")
fileNames = c("data/Pima.csv")
fileNames = c("data/bcw.csv")
fileNames = c("data/spam.csv")

# Répétition des jeux de données 
nbTests = 20

# Type de modèle 
model_global = c("Discriminat Analysis")
model_global = c("Logistic Regression")
model_global = c("Decision Tree")
model = c("Quadratic Discriminat Analysis")
model = c("Linear Discriminat Analysis")
model = c("Naive Bayes classifier")
model = c("Logistic Regression")
model = c("Quadratic Logistic Regression")

intr = T
epsi = 1e-5

# Calculation le taux d'erreur de test moyen
meanErrorRates <- function(fileNames, model_global, model) {
  detailledErrorRates = list()
  meanErrorRates = list()
  
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
      if(model_global == "Discriminat Analysis"){
        if (model == "Quadratic Discriminat Analysis") {
          params = adq.app(sample$Xapp, sample$zapp) # calculer les paramètres du modèle
        } 
        if (model == "Linear Discriminat Analysis") {
          params = adl.app(sample$Xapp, sample$zapp)
        }
        if (model == "Naive Bayes classifier") {
          params = nba.app(sample$Xapp, sample$zapp)
        }
        testPredictedClasses = ad.val(params, sample$Xtst) # prédire les classes du jeu de données de test
        testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$ztst, sample$ztst)
      }
      if (model_global == "Logistic Regression") {
        if (model == "Logistic Regression") {
          params = log.app(sample$Xapp, sample$zapp, intr, epsi)
        }
        if (model == "Quadratic Logistic Regression") {
          if(file == "data/bcw.csv") {
            cat("We don't use the method ",model," when the data is ",file,".\n")
            break
          }
          else
            params = log.app(sample$Xapp, sample$zapp, intr, epsi)
        }
        testPredictedClasses = log.val(params$beta,sample$Xtst)
        testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$pred, sample$ztst)
      }
      if (model_global == "Decision Tree") {
        testPredictedClasses <- tree.app(sample$Xapp,sample$zapp,sample$Xtst,sample$ztst)
        testErrorRate = 1 - compute.sucess.rate(testPredictedClasses$ztst, sample$ztst)
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
    
    cat("Method:", model,"\n")
    print(round(meanErrorRates[[file]], 3))
    writeLines("")
  }
}

meanErrorRates(fileNames,model_global,model)





