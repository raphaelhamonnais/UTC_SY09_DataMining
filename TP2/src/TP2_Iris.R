#pour exécuter la fonction diana
#install.packages("cluster")
library(cluster)

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
ColorDendrogram(iris_hclust, y = as.character(species_colors), branchlength = 7,  main = "Iris : classification hiérarchique ascendante", ylab = "Indice", xlab = "Individus classés")
legend(120,30, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
rect.hclust(iris_hclust, k = 3, border = c("red", "blue", "orange"))
#calculer "the adjusted Rand index"
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



#####################3.Méthode des centres mobiles####################
#3.1 Tenter une partition en K∈{2, 3, 4} classes avec la fonction kmeans;visualiser et commenter
#install.packages("ggplot2")
#library("ggplot2")
#ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
#la partition en deux classes
iris_kmeans_2 <- kmeans(iris_quant, 2)
iris_kmeans_2
#le individu de regroupement appartient 
iris_kmeans_2$cluster
# le point central de chaque variable dans chaque groupe
iris_kmeans_2$centers
#vérifier le nombre de iris dans chaque cluster 
table(iris$Species, iris_kmeans_2$cluster);
#Centre mobile en 2 clusters
#methode un
plot(iris_quant, col = iris_kmeans_2$cluster,pch = as.integer(iris$Species), main = "Centre mobile en 2 clusters")
#methode deux
clusplot(iris_quant, iris_kmeans_2$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Centre mobile en 2 clusters")
clusplot(iris_quant, iris_kmeans_2$cluster, color = T, shade = FALSE, labels = 0, main = "Centre mobile en 2 clusters")
#dessiner la relation entre le longueur et le largeur du sépale d'iris en regroupant par le nombre de cluster 
#plot(iris_quant[c("Sepal.Length","Sepal.Width")], col = iris_kmeans_2$cluster, pch = as.integer(iris$Species));
#Marquer la centre de gravité de chaque cluster, pch est un paramètre pour dessiner des points en utilisant différents types
#points(iris_kmeans_2$centers[,c("Sepal.Length","Sepal.Width")], col = 1:2, pch = 8, cex=3);
#dessiner la relation entre le longueur et le largeur du sépale d'iris en regroupant par le nombre de cluster 
#plot(iris_quant[c("Petal.Length","Petal.Width")], col = iris_kmeans_2$cluster, pch = as.integer(iris$Species));
#Marquer la centre de gravité de chaque cluster, pch est un paramètre pour dessiner des points en utilisant différents types
#points(iris_kmeans_2$centers[,c("Petal.Length","Petal.Width")], col = 1:2, pch = 7, cex=3);


#la partition en trois classes
iris_kmeans_3 <- kmeans(iris_quant, 3)
iris_kmeans_3
#le individu de regroupement appartient 
iris_kmeans_3$cluster
# le point central de chaque variable dans chaque groupe
iris_kmeans_3$centers
#vérifier le nombre de iris dans chaque cluster 
table(iris$Species, iris_kmeans_3$cluster);
#Centre mobile en 3 clusters
#methode un
plot(iris_quant, col = iris_kmeans_3$cluster,pch = as.integer(iris$Species), main = "Centre mobile en 3 clusters")
#methode deux
clusplot(iris_quant,iris_kmeans_3$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Centre mobile en 3 clusters")
clusplot(iris_quant,iris_kmeans_3$cluster, color = TRUE, shade = F, labels = 0, main = "Centre mobile en 3 clusters")
#dessiner la relation entre le longueur et le largeur du sépale d'iris en regroupant par le nombre de cluster 
#plot(iris_quant[c("Sepal.Length","Sepal.Width")], col = iris_kmeans_3$cluster, pch = as.integer(iris$Species));
#Marquer la centre de gravité de chaque cluster, pch est un paramètre pour dessiner des points en utilisant différents types
#points(iris_kmeans_3$centers[,c("Sepal.Length","Sepal.Width")], col = 1:3, pch = 8, cex=3);
#dessiner la relation entre le longueur et le largeur du sépale d'iris en regroupant par le nombre de cluster 
#plot(iris_quant[c("Petal.Length","Petal.Width")], col = iris_kmeans_3$cluster, pch = as.integer(iris$Species));
#Marquer la centre de gravité de chaque cluster, pch est un paramètre pour dessiner des points en utilisant différents types
#points(iris_kmeans_3$centers[,c("Petal.Length","Petal.Width")], col = 1:3, pch = 7, cex=3);


#la partition en quatre classes
iris_kmeans_4 <- kmeans(iris_quant, 4)
iris_kmeans_4
#le individu de regroupement appartient 
iris_kmeans_4$cluster
# le point central de chaque variable dans chaque groupe
iris_kmeans_4$centers
#vérifier le nombre de iris dans chaque cluster 
table(iris$Species, iris_kmeans_4$cluster);
#Centre mobile en 3 clusters
#methode un
plot(iris_quant, col = iris_kmeans_4$cluster,pch = as.integer(iris$Species), main = "Centre mobile en 4 clusters")
#methode deux
clusplot(iris_quant, iris_kmeans_4$cluster, color = TRUE, shade = TRUE, labels = 2, main = "Centre mobile en 4 clusters")
clusplot(iris_quant, iris_kmeans_4$cluster, color = TRUE, shade = F, labels = 0, main = "Centre mobile en 4 clusters")
#dessiner la relation entre le longueur et le largeur du sépale d'iris en regroupant par le nombre de cluster 
#plot(iris_quant[c("Sepal.Length","Sepal.Width")], col = iris_kmeans_4$cluster, pch = as.integer(iris$Species));
#Marquer la centre de gravité de chaque cluster, pch est un paramètre pour dessiner des points en utilisant différents types
#points(iris_kmeans_4$centers[,c("Sepal.Length","Sepal.Width")], col = 1:4, pch = 8, cex=3);
#dessiner la relation entre le longueur et le largeur du sépale d'iris en regroupant par le nombre de cluster 
#plot(iris_quant[c("Petal.Length","Petal.Width")], col = iris_kmeans_4$cluster, pch = as.integer(iris$Species));
#Marquer la centre de gravité de chaque cluster, pch est un paramètre pour dessiner des points en utilisant différents types
#points(iris_kmeans_4$centers[,c("Petal.Length","Petal.Width")], col = 1:4, pch = 7, cex=3);


#3.2 Effectuer plusieurs classifications des données en K = 3 classes

iris_intra <- 0
#for (i in 1:nrow(iris_kmeans_3$centers))
for (i in 1:10) {
  cat('Classifiction',i,'\n')
  iris_kmeans_3 <- kmeans(iris_quant,3)
  #print(iris_kmeans_3$cluster)
  #calculer l'inertie intra-classe
  #iris_intra <- iris_intra + (1/d1)*sum((iris_quant - iris_kmeans_3$centers[i,])^2)
  cat(iris_kmeans_3$withinss, '\n') #inertie intra classe
  #cat(table(iris$Species, iris_kmeans_3$cluster), '\n')
  print(table(iris$Species, iris_kmeans_3$cluster))
  #iris_intra <- iris_intra + iris_kmeans_3$withinss[i]
  #cat('L\'inertie intra-classe:',iris_intra,'\n')
  #plot(iris_quant, col = iris_kmeans_3$cluster, pch = as.integer(iris$Species))
  #clusplot(iris_quant, iris_kmeans_3$cluster, color = T, shade = FALSE, labels = 0, main = "")
  #Sys.sleep(2)
}

#3.3 Déterminer le nombre de classes optimal 
#(a) effectuer N = 100 classifications en prenant K = 2, 3, ...,10 classes
#créer une matrice pour mettre des valeurs des inerties intra-classes
iris_matrix <- matrix(0, nrow = 100, ncol = 9)
rownames(iris_matrix) <- rownames(iris_matrix, do.NULL = FALSE, prefix = "N")
colnames(iris_matrix) <- c("K=2","K=3","K=4","K=5","K=6","K=7","K=8","K=9","K=10")
iris_matrix
for(k in 2:10)
{
  for (N in 1:100)
  {
      iris_kmeans <- kmeans(iris_quant,k)
      iris_matrix[N,k-1] <- iris_kmeans$tot.withinss
  }
}
iris_matrix

#(b)Pour chaque valeur de K, calculer l'inertie intra-classe mininale
iris_matrix_min <- apply(iris_matrix, 2, min)
iris_matrix_min
plot(iris_matrix_min, type ='o', xaxt='n', xlab = "K", ylab = "l'inertie intra-classe minimale")
axis(side = 1, at = seq(1,9,1),labels = c(2:10))



# Proposer un nombre de classes à partir de ces informations, en utilisant la méthode du coude
# = étudier la décroissance du critère (inertie totale) en fonction du nombre de classes et choisir le nombre de classes
#    avant le premier saut significatif

# ==> 3 ou 4 classes




########## 4 Comparer les résultats de la partition obtenue par les centres mobiles avec la partition réelle des iris en trois groupes. #############
#la partition en trois classes avec inertie minimale
iris_kmeans_3 <- kmeans(iris_quant, 3)
while(iris_kmeans_3$tot.withinss != iris_matrix_min[2]) {
  iris_kmeans_3 <- kmeans(iris_quant, 3)
}
iris_kmeans_3$tot.withinss
table(iris$Species, iris_kmeans_3$cluster);
#Centre mobile en 3 clusters
clusplot(iris_quant,iris_kmeans_3$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 3 clusters")
legend(2.5,2.5, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)

