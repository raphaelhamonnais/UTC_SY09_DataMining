source("src/fonctions/mvdnorm.r")
source("src/fonctions/prob.ad.R")

appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]
zapp <- factor(zapp)
g = length(levels(zapp)) # nombre de classes
n = dim(Xapp)[1] # n = nombre d'individus total
p = dim(Xapp)[2] # p = nombre de variables
pi = matrix(nrow = g, ncol = p) # le proportion pour chaque classe
rownames(pi) = c('pi1','pi2')
colnames(pi) = colnames(Xapp)
mu = matrix(nrow = g, ncol = p) # le moyen pour chaque classe
rownames(mu) = c('mu1','mu2')
colnames(mu) = colnames(Xapp)
var = matrix(nrow = g, ncol = p) # la variance pour chaque classe
rownames(var) = c('var1','var2')
colnames(var) = colnames(Xapp)

# fonction pour calculer le proportion et l'espérance pour les trois modèles
pi_mu.app <- function(Xapp, zapp) {
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    nk = dim(dataClassK)[1] # nombre d'individus pour chaque classe
    piForClassK = nk/n 
    meanForClassK = apply(dataClassK, MARGIN = 2, mean)
    pi[k,] = piForClassK
    mu[k,] = meanForClassK
  }
  return(rbind(pi,mu))
}
pi_mu.app(Xapp,zapp)

# Fonction pour calculer l'Analyse discrimiante quadratique
adq.app <- function(Xapp, zapp) {
  pi_mu <- pi_mu.app(Xapp,zapp)
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    varForClassK = apply(dataClassK, MARGIN = 2, var)
    var[k,] = varForClassK
  }
  return(rbind(pi_mu,var))
}
adq.app(Xapp,zapp)

# Fonction pour calculer l'Analyse discrimiante linéaire 
adl.app <- function(Xapp, zapp) {
  pi_mu <- pi_mu.app(Xapp,zapp)
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    nk = dim(dataClassK)[1] # nombre d'individus pour chaque classe
    varForClassK = apply(dataClassK, MARGIN = 2, var)
    var[k,] = ((nk-1)*varForClassK)/(n-g) # la variance de l'estimateur sans biais pour chaque classe
  }
  var = colSums(var) # la variance de l'estimateur sans biais
  return(rbind(pi_mu,var))
}
adl.app(Xapp,zapp)

# Fonction pour calculer le classifieur bayésien naïf
nba.app <- function(Xapp, zapp) {
  pi_mu <- pi_mu.app(Xapp,zapp)
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    varForClassK = apply(dataClassK, MARGIN = 2, var)
    var[k,] = diag(diag(varForClassK))
  }
  return(rbind(pi_mu,var))
}
nba.app(Xapp,zapp)

#####################################################################################

# Fonction pour calculer l'Analyse discrimiante quadratique
adq.app <- function(Xapp, zapp) {
  zapp <- factor(zapp)
  g = length(levels(zapp)) # nombre de classes
  n = dim(Xapp)[1] # n = nombre d'individus total
  p = dim(Xapp)[2] # p = nombre de variables
  pi = matrix(nrow = g, ncol = p) # le proportion pour chaque classe
  rownames(pi) = c('pi1','pi2')
  colnames(pi) = colnames(Xapp)
  mu = matrix(nrow = g, ncol = p) # le moyen pour chaque classe
  rownames(mu) = c('mu1','mu2')
  colnames(mu) = colnames(Xapp)
  var = matrix(nrow = g, ncol = p) # la variance pour chaque classe
  rownames(var) = c('var1','var2')
  colnames(var) = colnames(Xapp)
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    nk = dim(dataClassK)[1] # nombre d'individus pour chaque classe
    piForClassK = nk/n 
    meanForClassK = apply(dataClassK, MARGIN = 2, mean)
    varForClassK = apply(dataClassK, MARGIN = 2, var)
    pi[k,] = piForClassK
    mu[k,] = meanForClassK
    var[k,] = varForClassK
  }
  #return(list(proportion=pi,esperance=mu,variance=var))
  return(rbind(pi,mu,var))
}

# Fonction pour calculer l'analyse discriminate linéaire 
adl.app<- function(Xapp, zapp) {
  zapp <- factor(zapp)
  g = length(levels(zapp)) # nombre de classes
  n = dim(Xapp)[1] # n = nombre d'individus
  p = dim(Xapp)[2] # p = nombre de variables
  pi = matrix(nrow = g, ncol = p) # le proportion pour chaque classe
  rownames(pi) = c('pi1','pi2')
  colnames(pi) = colnames(Xapp)
  mu = matrix(nrow = g, ncol = p) # le moyen pour chaque classe
  rownames(mu) = c('mu1','mu2')
  colnames(mu) = colnames(Xapp)
  var = matrix(nrow = g, ncol = p) # la variance pour chaque classe
  rownames(var) = c('var1','var2')
  colnames(var) = colnames(Xapp)
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    nk = dim(dataClassK)[1] # nombre d'individus pour chaque classe
    piForClassK = nk/n 
    meanForClassK = apply(dataClassK, MARGIN = 2, mean)
    varForClassK = apply(dataClassK, MARGIN = 2, var)
    pi[k,] = piForClassK
    mu[k,] = meanForClassK
    var[k,] = ((nk-1)*varForClassK)/(n-g) # la variance de l'estimateur sans biais pour chaque classe
  }
  var = colSums(var) # la variance de l'estimateur sans biais
  #return(list(proportion=pi,esperance=mu,variance=var))
  return(rbind(pi,mu,var))
}

# Fonction pour calculer le classifieur bayésien naïf 
nba.app<- function(Xapp, zapp) {
  zapp <- factor(zapp)
  g = length(levels(zapp)) # nombre de classes
  n = dim(Xapp)[1] # n = nombre d'individus
  p = dim(Xapp)[2] # p = nombre de variables
  pi = matrix(nrow = g, ncol = p) # le proportion pour chaque classe
  rownames(pi) = c('pi1','pi2')
  colnames(pi) = colnames(Xapp)
  mu = matrix(nrow = g, ncol = p) # le moyen pour chaque classe
  rownames(mu) = c('mu1','mu2')
  colnames(mu) = colnames(Xapp)
  var = matrix(nrow = g, ncol = p) # la variance pour chaque classe
  rownames(var) = c('var1','var2')
  colnames(var) = colnames(Xapp)
  for (k in 1:g) {
    dataClassK = Xapp[zapp == levels(zapp)[k],]
    nk = dim(dataClassK)[1] # nombre d'individus pour chaque classe
    piForClassK = nk/n 
    meanForClassK = apply(dataClassK, MARGIN = 2, mean)
    varForClassK = apply(dataClassK, MARGIN = 2, var)
    pi[k,] = piForClassK
    mu[k,] = meanForClassK
    var[k,] = diag(diag(varForClassK))
  }
  #return(list(proportion=pi,esperance=mu,variance=var))
  return(rbind(pi,mu,var))
}

ad.val <- function(Xtst, pi1, mu1, var1, pi2, mu2, var2) {
  
  
}

#################Test#######################
appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]
pi_mu <- pi_mu.app(Xapp, zapp)
adq <- adq.app(Xapp, zapp)
adl <- adl.app(Xapp,zapp)
nba <- nba.app(Xapp, zapp)

testData <- read.csv("data/Synth2-40.csv")
Xtst <- testData[,1:2]
ztst <- testData[,3]

mvdnorm(Xtst, pi_mu[3,], adq[5,])
class(adq[5,])
class(pi_mu[3,])
