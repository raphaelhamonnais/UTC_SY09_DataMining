source("src/fonctions-tp3/distXY.R")
source("src/fonctions-tp3/front.ceuc.R")
source("src/fonctions-tp3/front.kppv.R")




kppv.val <- function(Xapp, zapp, K, Xtst) {
  
  distXY(Xapp, Xtst)
  
}


kppv.tune <- function(Xapp, zapp, Xval, zval, nppv) {
  
  
}






testData = read.csv("data/Synth1-40.csv")
Xapp = testData[,1:2]
Xtst = testData[,1:2]
zapp = factor(testData[,3])
dist = distXY(Xtst, Xapp)
dist[1,]


a = c(35,6221,26,1,4)
K = 3
maxs = vector(length = K)
for (i in 1:K) {
  maxs[i] = which.max(a)
  a = a[-which.max(a)]
}
maxs

a[-c(1,2,0)]
a = apply(dist_, 1, which.min)
a
