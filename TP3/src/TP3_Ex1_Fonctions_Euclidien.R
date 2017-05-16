source("src/fonctions-tp3/distXY.R")
source("src/fonctions-tp3/front.ceuc.R")
source("src/fonctions-tp3/front.kppv.R")

ceuc.app <- function(Xapp, zapp) {
  zapp = factor(zapp)
  g = length(levels(zapp)) # nb de classes
  p = dim(Xapp)[2] # p = nb colonnes
  result = matrix(nrow = g, ncol = p)
  rownames(result) = levels(zapp)
  colnames(result) = colnames(Xapp)
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    meanForClassK = apply(dataClassK, MARGIN = 2, mean)
    result[k,] = meanForClassK
  }
  result
}

ceuc.val <- function(mu, Xtst) {
  g = dim(mu)[1] # nb classes = nb lignes paramètres mu
  p = dim(Xtst)[2] # nb variables = nb colonnes
  n = dim(Xtst)[1] # nb individus de test
  result = vector(length = n)
  classes = factor(rownames(mu))
  distances = distXY(Xtst, mu)
  intClasses = apply(distances, MARGIN = 1, which.min) # get class with minimum distance to gravity center
  result = levels(classes)[intClasses] # get real class names
  factor(result) # return results in factor form
}






# TESTS persos
testData = read.csv("data/Synth1-40.csv")
Xapp = testData[,1:2]
zapp = factor(testData[,3])
mu = ceuc.app(quant, classes)
front.ceuc(mu, Xapp, zapp, 1000)

ceuc.val(mu, Xapp)
