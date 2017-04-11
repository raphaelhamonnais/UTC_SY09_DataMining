#pour exécuter la fonction diana
#install.packages("cluster")
library(cluster)

#pour ajouter différents couleurs dans un dendrogramme 
#install.packages("sparcl")
library("sparcl")

# For the methods map values
install.packages("plyr")
library(plyr)

# For rand index
install.packages("mclust")
library(mclust)



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
iris_hclust

#faire le "heatmap" pour vérifier globalement le découpage des données
#ici on voit qu'il y a deux parties de couleur différents, donc on découpe les données en deux groupes. 
heatmap(as.matrix(iris_dist), labRow = F, labCol = F)

#afficher le dendrogramme 
plot(iris_hclust, labels = FALSE)

#afficher le dendrogramme avec des cadres rectangulaires
rect.hclust(iris_hclust, k = 2)

# 3 espèces = 3 classes
rect.hclust(iris_hclust, k = 3)

#séparer les trois espèces en differentes couleurs
species_colors = mapvalues(iris$Species, from = c("setosa", "versicolor", "virginica"), to = c("red", "orange", "blue"))

ColorDendrogram(iris_hclust, y = as.character(species_colors), branchlength = 7,  main = "Iris : classification hiérarchique ascendante", ylab = "Indice", xlab = "Individus classés")
legend(120,30, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
rect.hclust(iris_hclust, k = 3, border = c("red", "blue", "orange"))
adjustedRandIndex(cutree(iris_hclust, k = 3), iris$Species)

#2.3 effectuer la classification hiérarchique descendante 
iris_diana <- diana(iris_dist)
iris_diana
#iris_cutre_diana <- cutree(iris_diana, k = 3)
#iris_cutre_diana
#plot(iris_cutre_diana, col = c("darkblue", "darkorange", "darkgreen")[iris$Species])
ColorDendrogram(as.hclust(iris_diana), y = as.character(species_colors), branchlength = 7, main = "Iris : classification hiérarchique descendante", ylab = "Indice", xlab = "Individus classés")
legend(120,7, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
rect.hclust(as.hclust(iris_diana), k = 3, border = c("red", "orange", "blue"))
adjustedRandIndex(cutree(as.hclust(iris_diana), k = 3), iris$Species)



# Résultat = Diana moins bon car rand = 0,69 au lieu de 0,73
