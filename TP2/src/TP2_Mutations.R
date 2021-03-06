library(xtable)

#For the function Shepard
#install.packages("MASS")
library(MASS)

#For the function clusplot
#install.packages("cluster")
library(cluster)

######## Initialisation données ##########
mut <- read.csv("data/mutations2.csv", header=T, row.names = 1)
mut
#plot(mut)
summary(mut)
dim(mut)
d1 <- dim(mut)[1]
d1
#faire la tableau de dissimilarite
mut <- as.dist(mut, diag = T, upper = T)
mut


######## 1. Représentation graphique ##########
# Il s'agit de calculer la représentation euclidienne des donnees par AFTD (analyse factorielle 
#   de tableau de distance) en k dimensions.

###### Fonctions pour calculer les % d'inertie expliquée #########
create_inertie_table = function(x, nbCol) {
  inertExpliquee.cumulated = x
  for (i in 2:length(inertExpliquee.cumulated)) {
    inertExpliquee.cumulated[i] = inertExpliquee.cumulated[i] + inertExpliquee.cumulated[i-1]
  }
  total = rbind(x, inertExpliquee.cumulated)
  rownames(total) = c("Pourcentage d'inertie expliquée", "Pourcentage cumulé d'inertie expliquée")
  total
}
calc_abs_inertie_explique = function(x, nbDimensions=2) {
  aftd_mut.inertExpliqueeAbs = abs(x) / sum(abs(x)) * 100
  total = create_inertie_table(aftd_mut.inertExpliqueeAbs, nbDimensions)
  total[,1:nbDimensions]
}
calc_inertie_explique_witout_neg_values = function(x, nbDimensions=2) {
  valPWithoutNegValues = x
  for (i in 1:length(valPWithoutNegValues)) {
    if (valPWithoutNegValues[i] < 0)
      valPWithoutNegValues[i] = 0;
  }
  inertExpliqueeWithoutNegValues = valPWithoutNegValues/sum(valPWithoutNegValues)*100
  total = create_inertie_table(inertExpliqueeWithoutNegValues, nbDimensions)
  total[,1:nbDimensions]
}

# Calcul des pourcentages d'inertie pour k = 5 dimensions
k=5
aftd_mut <- cmdscale(mut, k = 5, eig = TRUE, x.ret = TRUE)
inert_abs = round(calc_abs_inertie_explique(aftd_mut$eig, k), 2)
colnames(inert_abs) = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5")
inert_abs
inert_posi = round(calc_inertie_explique_witout_neg_values(aftd_mut$eig, k), 2)
colnames(inert_posi) = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5")
inert_posi


# Représentation graphique pour k = 2 dimensions
k = 2
aftd_mut <- cmdscale(mut, k = 2, eig = TRUE, x.ret = TRUE) # eig = avoir les données résultantes de la diagonalisation par eigen x.ret = avoir la matrice doublement centrée W des produits scalaires 
#représentation graphique
aftd_mut$points # représente les composantes principales trouvées avec ACP
plot(aftd_mut$points, xlab = "Comp.1", ylab = "Comp.2") # toutes les données
plot(aftd_mut$points[1:17,], xlab = "Comp.1", ylab = "Comp.2") # sans les données extrêmes pour voir s'il y a des classes dans les données regroupées

# Diagrammes de shepard pour k allant de 2 à 5 dimensions
##### k = 2 dimensions
aftd_mut_2 <- cmdscale(mut, k = 2, eig = TRUE, x.ret = TRUE)
dist(aftd_mut_2$points,diag = T, upper = T) # fonction pour transformer la représentation des points sur les axes en une matrice de distance
plot(mut, dist(aftd_mut_2$points, diag = T, upper = T), xlab = "Dissimilarités originelles", ylab = "Dissimilarités espace factoriel à 2 dimensions")
abline(1,1)
##### k = 3 dimensions
aftd_mut_3 <- cmdscale(mut, k = 3, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_mut_3$points, diag = T, upper = T), xlab = "Dissimilarités originelles", ylab = "Dissimilarités espace factoriel à 3 dimensions")
abline(1,1)
##### k = 4 dimensions ######
aftd_mut_4 <- cmdscale(mut, k = 4, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_mut_4$points, diag = T, upper = T), xlab = "Dissimilarités originelles", ylab = "Dissimilarités espace factoriel à 4 dimensions")
abline(1,1)
##### k = 5 dimensions ######
aftd_mut_5 <- cmdscale(mut, k = 5, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_mut_5$points, diag = T, upper = T), xlab = "Dissimilarités originelles", ylab = "Dissimilarités espace factoriel à 5 dimensions")
abline(1,1)


################## 2. classification hiérarchique ###################
#2.1 faire un cluster pour effectuer la classification hiérarchique ascendante 
#ici la méthode qu'on utilise est la méthode de ward
mut_hclust <- hclust(mut, method = "ward.D2")
mut_hclust

#faire le "heatmap" pour vérifier globalement le découpage des données
#ici on voit qu'il y a trois parties de couleur différents, donc on découpe les données en trois groupes. 
heatmap(as.matrix(mut),labRow = F, labCol = F)

#afficher le dendrogramme 
plot(mut_hclust)

#afficher le dendrogramme avec des cadres rectangulaires
rect.hclust(mut_hclust, k = 3)

#découpage en 3 groupes
mut_cutree <- cutree(mut_hclust, k = 3)
t(t(mut_cutree))


######### # classificaiton avec différents critères d'agrégation #########

# min - Distance inter-classes = distance minimum entre leurs objets respectifs les plus proches
# type de classes = spectrum or chain (chaîne)
mut_hclust = hclust(mut, method = "single")
plot(mut_hclust, hang = -1, main = "Critère d'agrégation \"Min\"", xlab = "Espèces", ylab = "Valeur d'indice", sub = "")
#rect.hclust(mut_hclust, k = 2)
rect.hclust(mut_hclust, k = 3)

# max - Distance inter-classes = distance maximum entre leurs objets respectifs les plus distants
# types de classes = circle (by hobby, plot) (cercle de connaissance, de passion commune ?)
mut_hclust = hclust(mut, method = "complete")
plot(mut_hclust, hang = -1, main = "Critère d'agrégation \"Max\"", xlab = "Espèces", ylab = "Valeur d'indice", sub = "")
rect.hclust(mut_hclust, k = 3)

# moyenne - UPGMA - Distance inter-classe = moyenne arithmétique de toutes les distances entre les objets des deux classes  
# souvent méthode par défaut
# type de classe générique
mut_hclust = hclust(mut, method = "average")
plot(mut_hclust, hang = -1, main = "Critère d'agrégation \"Moyenne\"", xlab = "Espèces", ylab = "Valeur d'indice", sub = "")
rect.hclust(mut_hclust, k = 3)

# WPGMA - Simple average, or method of equilibrious between-group average - Même chose que "moyenne" (UPGMA) 
#   sauf que que les sous-classes de la dernière classe ayant fusionée ont une importante égale, indifférement 
#   de leur taille en termes d'effectifs (normalisation par rapport aux effectifs des deux classes qui ont été 
#   regroupées précédemment)
# # type de classe générique
mut_hclust = hclust(mut, method = "mcquitty")
plot(mut_hclust, hang = -1, main = "Critère d'agrégation \"Moyenne pondérée\"", xlab = "Espèces", ylab = "Valeur d'indice", sub = "")
rect.hclust(mut_hclust, k = 3)


# UPGMC - Distance inter-classe = distance euclidienne entre leurs centre de gravité respectifs
#   méthode qui n'a pas un indice strictement croissant/décroissant ???
# type de classes = "proximity of platforms (politics)"
mut_hclust = hclust(mut, method = "centroid")
plot(mut_hclust, hang = -1, main = "Critère d'agrégation \"UPGMC : Distance euclidienne \n entre les centres de gravité des classes\"", xlab = "Espèces", ylab = "Valeur d'indice", sub = "")
#rect.hclust(mut_hclust, k = 2)
rect.hclust(mut_hclust, k = 3)

# WPGMC - "centroid" modifiée - Distance inter-classe = distance entre leur centre de gravité respectifs sauf 
#   que que les sous-classes de la dernière classe ayant fusionée ont une importante égale, indifférement de 
#   leur taille en termes d'effectifs (normalisation par rapport aux effectifs des deux classes qui ont été 
#   regroupées précédemment)
# type de classes = "proximity of platforms (politics)"
#   méthode qui n'a pas un indice strictement croissant/décroissant => perd "monotonie"
mut_hclust = hclust(mut, method = "median")
plot(mut_hclust, hang = -1, main = "Critère d'agrégation \"WPGMC : UPGMC pondéré\"", xlab = "Espèces", ylab = "Valeur d'indice", sub = "")
#rect.hclust(mut_hclust, k = 2)
rect.hclust(mut_hclust, k = 3)

# Ward’s method, or minimal increase of sum-of-squares (MISSQ), sometimes incorrectly called "minimum variance" method.
#   Proximity between two clusters is the magnitude by which the summed square in their joint cluster will be 
#   greater than the combined summed square in these two clusters: SS(1,2) − (  SS(1) + SS(2)  ).
#   (Between two singleton objects this quantity = squared euclidean distance / 2.)
# Classe mettant en valeur le "type" des objets de la classe
mut_hclust = hclust(mut, method = "ward.D2")
plot(mut_hclust, hang = -1, main = "Critère d'agrégation \"Ward\"", xlab = "Espèces", ylab = "Valeur d'indice", sub = "")
#rect.hclust(mut_hclust, k = 2)
rect.hclust(mut_hclust, k = 3)



####################### 3 méthode des centres mobiles #############################
#3.1 K = 3 classes
aftd_mut_5 <- cmdscale(mut, k = 5, eig = TRUE, x.ret = TRUE)
aftd_mut_5
aftd_mut_5$points
aftd_mut_5$x
mut_kmeans_3 <- kmeans(aftd_mut_5$points,3)
#mut_kmeans_3
#mut_kmeans_3$cluster
plot(aftd_mut_5$points,  col = c("red","blue", "green")[mut_kmeans_3$cluster]) #TODO demander au prof
clusplot(aftd_mut_5$points, mut_kmeans_3$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 0, main = "Centre mobile en 3 clusters")
mut_kmeans_3$tot.withinss
#3.2 étudier la stabilité du résultat de la partition 
#mut_matrix <- matrix(0, nrow = 100, ncol = 1)
#rownames(mut_matrix) <- rownames(mut_matrix, do.NULL = FALSE, prefix = "N")
#colnames(mut_matrix) <- c("K=2","K=3","K=4","K=5","K=6","K=7","K=8","K=9","K=10")
#mut_matrix
#for(k in 2:10) {
#  for (N in 1:100) {
#    mut_kmeans <- kmeans(aftd_mut_5$points,k)
#    mut_matrix[N,k-1] <- mut_kmeans$tot.withinss
#  }
#}
#mut_matrix
# Stabilité : 
#t(t(table(mut_matrix[,1])))
#t(t(table(mut_matrix[,2])))
#t(t(table(mut_matrix[,3])))
n = dim(aftd_mut_5$points)[1]
roundN = 3
nbComparaisons=1000
classifications_seen = vector(length = nbComparaisons)
for (i in 1:nbComparaisons) {
  mut_kmeans <- kmeans(aftd_mut_5$points, 3)
  classifications_seen[i] = round(mut_kmeans$tot.withinss / n, digits = roundN)
}
classifications_seen = sort(classifications_seen, decreasing = F)
uq_classifications_seen = unique(classifications_seen)
t(t(table(round(classifications_seen, 3))/1000*100))
xtable(t(t(table(round(classifications_seen, 3))/1000*100)))
uq_classifications_seen

mut_kmeans = getExpectedInertie(tabQuant = aftd_mut_5$points, nbClasses = 3, roundN, expectedInertie = 3621.850)
clusplot(aftd_mut_5$points, mut_kmeans$cluster ,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "", cex.txt = 0.5)

mut_kmeans = getExpectedInertie(tabQuant = aftd_mut_5$points, nbClasses = 3, roundN, expectedInertie = 4398.199)
clusplot(aftd_mut_5$points, mut_kmeans$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "", cex.txt = 0.5)

mut_kmeans = getExpectedInertie(tabQuant = aftd_mut_5$points, nbClasses = 3, roundN, expectedInertie = 4918.086)
clusplot(aftd_mut_5$points, mut_kmeans$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "", cex.txt = 0.5)

mut_kmeans = getExpectedInertie(tabQuant = aftd_mut_5$points, nbClasses = 3, roundN, expectedInertie = 4974.990)
clusplot(aftd_mut_5$points, mut_kmeans$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "", cex.txt = 0.5)

mut_kmeans = getExpectedInertie(tabQuant = aftd_mut_5$points, nbClasses = 3, roundN, expectedInertie = 5092.462)
clusplot(aftd_mut_5$points, mut_kmeans$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "", cex.txt = 0.5)

mut_kmeans = getExpectedInertie(tabQuant = aftd_mut_5$points, nbClasses = 3, roundN, expectedInertie = 5237.229)
clusplot(aftd_mut_5$points, mut_kmeans$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "", cex.txt = 0.5)

