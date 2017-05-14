############# Packages #################
#pour exécuter la fonction diana
#install.packages("cluster")
library(cluster)
library(xtable)

#pour ajouter différents couleurs dans un dendrogramme 
#install.packages("sparcl")
library("sparcl")

# For the methods map values
#install.packages("plyr")
library(plyr)

# For rand index
#install.packages("mclust")
library(mclust)

#For thé function points 
#install.packages("graphicsQC")
library("graphicsQC")

#Charger les données Iris 
data(iris)
iris
summary(iris)
#Supprimer le paramètre "Species" 
iris_quant <- iris[,-5]
iris_quant
dim(iris_quant)
#Obtenir le nombre de ligne des données 
d1 <- dim(iris_quant)[1]
d1


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
legend(2.5, 2, legend=c(levels(iris$Species)), col=c("darkblue", "darkorange", "darkgreen"), pch=16, cex=.8)


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

#faire un dendrogramme 
ColorDendrogram(iris_hclust, y = as.character(species_colors), branchlength = 7,  main = "Iris : classification hiérarchique ascendante", ylab = "Indice", xlab = "Individus classés", sub = "")
legend(120,30, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
rect.hclust(iris_hclust, k = 3, border = c("red", "blue", "orange"))
#calculer "the adjusted Rand index"
adjustedRandIndex(cutree(iris_hclust, k = 3), iris$Species)

#2.3 effectuer la classification hiérarchique descendante 
iris_diana <- diana(iris_dist)
iris_diana
#iris_cutre_diana <- cutree(iris_diana, k = 3)
#iris_cutre_diana
ColorDendrogram(as.hclust(iris_diana), y = as.character(species_colors), branchlength = 7, main = "Iris : classification hiérarchique descendante", ylab = "Indice", xlab = "Individus classés", sub = "")
legend(120,7, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
rect.hclust(as.hclust(iris_diana), k = 3, border = c("red", "orange", "blue"))
adjustedRandIndex(cutree(as.hclust(iris_diana), k = 3), iris$Species)

# Résultat = Diana moins bon car rand = 0,69 au lieu de 0,73





#####################3.Méthode des centres mobiles####################
#3.1 Tenter une partition en K∈{2, 3, 4} classes avec la fonction kmeans;visualiser et commenter
iris_kmeans_2 <- kmeans(iris_quant, 2)
iris_kmeans_2$cluster # clusterisation
iris_kmeans_2$centers # le point central de chaque cluster
table(iris$Species, iris_kmeans_2$cluster) # répartition des individus dans une classe
clusplot(iris_quant, iris_kmeans_2$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(2.85,3.2, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.8,3.2, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(iris_kmeans_2$cluster)], cex=.8)

#la partition en trois classes
iris_kmeans_3 <- kmeans(iris_quant, 3)
iris_kmeans_3$tot.withinss / dim(iris_quant)[1]
table(iris$Species, iris_kmeans_3$cluster);
clusplot(iris_quant, iris_kmeans_3$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "K = 3")
legend(2.75,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.8,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_3$cluster)], cex=.8)

#la partition en quatre classes
iris_kmeans_4 <- kmeans(iris_quant, 4)
table(iris$Species, iris_kmeans_4$cluster);
clusplot(iris_quant, iris_kmeans_4$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "K = 4")
legend(2.75,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.8,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3","Cluster 4"), pch = c(1,2,3,4)[unique(iris_kmeans_4$cluster)], cex=.8)


#3.2 Effectuer plusieurs classifications des données en K = 3 classes
test_stability_kmeans = function(tab_quant, K, nbComparaisons, originalClasses, latexTable) {
  comparaison = matrix(nrow = nbComparaisons, ncol = 1)
  colnames(comparaison) = c("Inertie intra classe")
  row.names(comparaison) = c(1:nbComparaisons)
  classifications_seen = vector(length = nbComparaisons)
  n = dim(tab_quant)[1]
  
  for (i in 1:nbComparaisons) {
    k_means_result = kmeans(tab_quant,K)
    inertie = round(k_means_result$tot.withinss / n, 2)
    comparaison[i,1] = inertie
    if (! is.element(inertie, classifications_seen)) {
      classifications_seen[i] = inertie
      cat('\n\nClassification',i)
      if (latexTable)
        print(xtable(table(originalClasses, k_means_result$cluster)))
      else
        print(table(originalClasses, k_means_result$cluster))
    }
  }
  if (latexTable) {
    print(xtable(unique(comparaison)))
    print(xtable(table(comparaison)))
  }
  else {
    print((unique(comparaison)))
    print((table(comparaison)))
  }
}

test_stability_kmeans(iris_quant, K = 3, nbComparaisons = 100, originalClasses =  iris$Species, latexTable = F)


#3.3 Déterminer le nombre de classes optimal 
#(a) effectuer N = 100 classifications en prenant K = 2, 3, ...,10 classes
#créer une matrice pour mettre des valeurs des inerties intra-classes
iris_matrix <- matrix(0, nrow = 100, ncol = 9)
rownames(iris_matrix) <- rownames(iris_matrix, do.NULL = FALSE, prefix = "N")
colnames(iris_matrix) <- c("K=2","K=3","K=4","K=5","K=6","K=7","K=8","K=9","K=10")
iris_matrix
for(k in 2:10) {
  for (N in 1:100) {
      iris_kmeans <- kmeans(iris_quant,k)
      iris_matrix[N,k-1] <- iris_kmeans$tot.withinss / dim(iris_quant)[1]
  }
}

#(b)Pour chaque valeur de K, calculer l'inertie intra-classe mininale
iris_matrix_min <- apply(iris_matrix, 2, min)
iris_matrix_min

# Proposer un nombre de classes à partir de ces informations, en utilisant la méthode du coude
# = étudier la décroissance du critère (inertie totale) en fonction du nombre de classes et choisir le nombre de classes
#    avant le premier saut significatif
plot(iris_matrix_min, type ='o', xaxt='n', xlab = "Nombe K de classes", ylab = "Inertie intra-classe minimale")
axis(side = 1, at = seq(1,9,1),labels = c(2:10))
# ==> 3 ou 4 classes




########## 4 Comparer les résultats de la partition obtenue par les centres mobiles avec la partition réelle des iris en trois groupes. #############
#la partition en trois classes avec inertie minimale
iris_kmeans_3 <- kmeans(iris_quant, 3)
while((iris_kmeans_3$tot.withinss / dim(iris_quant)[1]) != iris_matrix_min[2]) {
  iris_kmeans_3 <- kmeans(iris_quant, 3)
}
iris_kmeans_3$tot.withinss
table(iris$Species, iris_kmeans_3$cluster);
#Centre mobile en 3 clusters
clusplot(iris_quant,iris_kmeans_3$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 3 clusters")
legend(2.5,2.5, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.8,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_3$cluster)], cex=.8)
