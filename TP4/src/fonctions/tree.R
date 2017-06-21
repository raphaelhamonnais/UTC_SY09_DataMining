#install.packages("tree")
#install.packages("rpart")
#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
library("tree")


#tree.app <- function(Xapp, zapp, Xtst)
#{
#    zapp = factor(zapp)
#    # obtenir l'arbre en utilisant le parametre control
#    control = tree.control(nobs=dim(Xapp)[1],mindev=0.0001)
#    # faire l'apprentissage des arbres de decision
#    tree = tree(zapp~., Xapp, control = control)
#    # constuire la sequence de'arbres emboites et d'estimeer l'erreur par validation croisee
#    cv = cv.tree(tree)
#    # cv$dev est les erreurs estimees par validation croissee 
#    bestSize = cv$size[which.min(cv$dev)]
#    # elaguer l'arbre 
#    prune_tree = prune.misclass(tree, best = bestSize)
#    # classer des donnees de test au moyen d'un arbre
#    testPred <- predict(prune_tree,newdata = Xtst)
#    
#    # classer des donnees du test selon le resultat predicted
#    #plot(tree)
#    #text(tree)
#    pred <- max.col(testPred)
#    out <- NULL
#    out$pred <- factor(pred)
#    out$fullTree = tree
#    out$pruneTree = prune_tree
#    return(out)
#}
