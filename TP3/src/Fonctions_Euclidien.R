source("src/fonctions-tp3/distXY.R")
source("src/fonctions-tp3/front.ceuc.R")
source("src/fonctions-tp3/front.kppv.R")
source("src/Fonctions_Utilities.R")

ceuc.app <- function(Xapp, zapp) {
  zapp = factor(zapp)
  g = length(levels(zapp)) # nb de classes
  p = dim(Xapp)[2] # p = nb colonnes
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
  intClasses = apply(distances, MARGIN = 1, which.min) # récupérer les classes dont on est le plus proche du centre de gravité
  predictedClasses = levels(classes)[intClasses] # obtenir le nom réel des classes
  return(factor(predictedClasses))
}


testData = read.csv("data/Synth2-1000.csv")
testData = read.csv("data/Synth1-1000.csv")
Xapp = testData[,1:2]
zapp = factor(testData[,3])
mu = ceuc.app(Xapp, zapp)
front.ceuc(mu, Xapp, zapp, 100)

#ceuc.val(mu, Xapp)
