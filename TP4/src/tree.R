#install.packages("tree")
library("tree")

tree.app <- function(Xapp,zapp,Xtst,ztst)
{
  zapp = factor(zapp)
  # obtenir l'arbre en utilisant le parametre control
  control = tree.control(nobs=dim(Xapp)[1],mindev=0.0001)
  # faire l'apprentissage des arbres de decision
  tree = tree(zapp~.,Xapp,control = control)
  # constuire la sequence de'arbres emboites et d'estimeer l'erreur par validation croisee
  cv = cv.tree(tree)
  # cv$dev est les erreurs estimees par validation croissee 
  bestSize = cv$size[which.min(cv$dev)]
  # elaguer l'arbre 
  prune_tree = prune.misclass(tree, best = bestSize)
  # classer des donnees de test au moyen d'un arbre
  testPred <- predict(prune_tree,newdata = Xtst)
  
  # classer des donnees du test selon le resultat predicted
  n = dim(Xtst)[1] # nombre d'individu des donnees du test
  
  for(i in 1 : n) {
    if(testPred[i,1] > 0.5)
      ztst[i] = 1
    else
      ztst[i] = 2
  }
  
  test_classed = cbind(Xtst,ztst)
  
  return(test_classed)
}

##############Test#######################
appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]

testData <- read.csv("data/Synth1-40.csv")
Xtst <- testData[,1:2]
ztst <- testData[,3]

tree.app(Xapp,zapp,Xtst,ztst)

