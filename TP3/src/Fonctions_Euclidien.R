source("src/fonctions-tp3/distXY.R")
source("src/fonctions-tp3/front.ceuc.R")
source("src/fonctions-tp3/front.kppv.R")
source("src/Fonctions_Utilities.R")

ceuc.app <- function(Xapp, zapp) {
  zapp = factor(zapp)
  g = length(levels(zapp)) # nombre de classes
  p = dim(Xapp)[2] # p = nombre de variables
  mu = matrix(nrow = g, ncol = p)
  rownames(mu) = levels(zapp)
  colnames(mu) = colnames(Xapp)
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    meanForClassK = apply(dataClassK, MARGIN = 2, mean)
    mu[k,] = meanForClassK
  }
  return(mu)
}

ceuc.val <- function(mu, Xtst) {
  g = dim(mu)[1] # nb classes = nb lignes paramètres mu
  p = dim(Xtst)[2] # nb variables = nb colonnes
  n = dim(Xtst)[1] # nb individus de test
  predictedClasses = vector(length = n)
  classes = factor(rownames(mu))
  distances = distXY(Xtst, mu)
  # récupérer les classes dont on est le plus proche du centre de gravité
  intClasses = apply(distances, MARGIN = 1, which.min)
  # obtenir le nom réel des classes
  predictedClasses = levels(classes)[intClasses]
  return(factor(predictedClasses))
}


appData = read.csv("data/Synth1-1000.csv")
Xapp = appData[,1:2]
zapp = factor(appData[,3])
mu = ceuc.app(Xapp, zapp)
round(mu, digits = 2)

testData = read.csv("data/Synth1-40.csv")
Xaff = testData[,1:2]
zaff = factor(testData[,3])
front.ceuc(mu, Xaff, zaff, 500)


