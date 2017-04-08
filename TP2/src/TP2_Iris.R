#pour exécuter la fonction diana
install.packages("cluster")
library(cluster)

data(iris)
iris
summary(iris)
iris_quant <- iris[,-5]
iris_quant
dim(iris_quant)



######## 1 - affichage dans le premier plan factoriel #########

#faire l'acp 
acp_iris <- princomp(iris_quant)
acp_iris
#affiche le premier plan factoriel sans tenir compte de l'espece
# plot(acp_iris$scores, asp = 1, xlab = "", ylab = "", cex.axis = 1.5, cex=1.5) # affichage pour hi-res
# title(xlab = "Comp.1", ylab = "Comp.2", line = 2.5, cex.lab = 2)
plot(acp_iris$scores, asp = 1)
##affiche le premier plan factoriel en tenant compte de l'espece
plot(acp_iris$scores, col = c("darkblue", "darkorange", "darkgreen")[iris$Species], asp = 1)
legend(2.5, 2.5, legend=c(levels(iris$Species)), col=c("darkblue", "darkorange", "darkgreen"), pch=16, cex=.8)


######## 2 - classification hiérarchique #################
#2.2 effectuer la classification hiérarchique ascendante 

#faire la matrice des distances entre des individus
iris_dist <- dist(iris_quant)
iris_dist

#faire un cluster pour effectuer la classification hiérarchique ascendante 
#ici la méthode qu'on utilise est la méthode de ward
iris_hclust <- hclust(iris_dist, method = "ward.D2")

#faire le "heatmap" pour vérifier globalement le découpage des données
#ici on voit qu'il y a deux parties de couleur différents, donc on découpe les données en deux groupes. 
heatmap(as.matrix(iris_dist), labRow = F, labCol = F)

#afficher le dendrogramme 
plot(iris_hclust, labels = FALSE)


#afficher le dendrogramme avec des cadres rectangulaires
rect.hclust(iris_hclust, k = 2)

# 3 espèces = 3 classes
rect.hclust(iris_hclust, k = 3)

#découpage en 2 groupes
iris_cutree <- cutree(iris_hclust, k = 2)
iris_cutree <- cutree(iris_hclust, k = 3)
iris_cutree
plot(iris_cutree, col = c("darkblue", "darkorange", "darkgreen")[iris$Species])

#2.3 effectuer la classification hiérarchique descendante 
iris_diana <- diana(iris_dist)
iris_diana
plot(iris_diana)
iris_cutre_diana <- cutree(iris_diana, k = 3)
iris_cutre_diana
plot(iris_cutre_diana, col = c("darkblue", "darkorange", "darkgreen")[iris$Species])










plot(iris_diana$order, col = c("darkblue", "darkorange", "darkgreen")[iris$Species])
plot(iris_hclust$order, col = c("darkblue", "darkorange", "darkgreen")[iris$Species])
plot(iris_diana$height, col = c("darkblue", "darkorange", "darkgreen")[iris$Species])
plot(iris_hclust$height, col = c("darkblue", "darkorange", "darkgreen")[iris$Species])
