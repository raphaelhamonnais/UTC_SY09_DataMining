source("src/fonctions/mvdnorm.r")
source("src/fonctions/prob.ad.R")

appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- factor(appData[,3])
zapp <- factor(zapp)
g = length(levels(zapp)) # nombre de classes
n = dim(Xapp)[1] # n = nombre d'individus total
p = dim(Xapp)[2] # p = nombre de variables

# calcul des proportions
prop.app = function(zapp) {
    zapp = factor(zapp)
    prop = list()
    for (k in 1:g) {
        prop[[k]] = table(zapp)[k] / length(zapp)
    }
    return(prop)
}
# calcul des centres de gravité
mu.app = function(Xapp, zapp) {
    zapp = factor(zapp)
    mu = list()
    for (k in 1:g) {
        dataClassK = Xapp[zapp == levels(zapp)[k],]
        mu[[k]] = apply(dataClassK, MARGIN = 2, mean)
    }
    return(mu)
}
# calcul des matrices de variance
sigma.app = function(Xapp, zapp) {
    zapp = factor(zapp)
    sigma = list()
    for (k in 1:g) {
        dataClassK = Xapp[zapp == levels(zapp)[k],]
        sigma[[k]] = var(dataClassK)
    }
    return(sigma)
}



adq.app <- function(Xapp, zapp) {
    params = list()
    params[["pi"]] = prop.app(zapp)
    params[["mu"]] = mu.app(Xapp, zapp)
    params[["sigma"]] = sigma.app(Xapp, zapp)
    return(params)
}
params = adq.app(Xapp,zapp)
params$pi
params$mu



adl.app <- function(Xapp, zapp) {
    params = list()
    prop = prop.app(zapp)
    mu = mu.app(Xapp, zapp)
    classes_sigma = sigma.app(Xapp, zapp)
    sigma = matrix(0, g, g)
    for (k in 1:g)
        sigma = sigma + (prop[[k]] * classes_sigma[[k]])
    params[["pi"]] = prop
    params[["mu"]] = mu
    params[["sigma"]] = sigma
    return(params)
}
params = adl.app(Xapp,zapp)
params$pi
params$mu
params$sigma


nba.app <- function(Xapp, zapp) {
    params = list()
    prop = prop.app(zapp)
    mu = mu.app(Xapp, zapp)
    classes_sigma = sigma.app(Xapp, zapp)
    for (k in 1:g)
        classes_sigma[[k]] = diag(diag(classes_sigma[[k]]))
    params[["pi"]] = prop
    params[["mu"]] = mu
    params[["sigma"]] = classes_sigma
    return(params)
}
params = nba.app(Xapp,zapp)
params$pi
params$mu
params$sigma
