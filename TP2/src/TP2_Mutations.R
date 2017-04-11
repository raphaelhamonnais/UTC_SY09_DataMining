library(xtable)


######## Initialisation données ##########

mut <- read.csv("data/mutations2.csv", header=T, row.names = 1)
mut
#plot(mut)
summary(mut)
dim(mut)
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
  aftd_mut.inertExpliqueeAbs = abs(aftd_mut.valP) / sum(abs(aftd_mut.valP)) * 100
#  aftd_mut.inertExpliqueeAbs.cumulated = aftd_mut.inertExpliqueeAbs
#  for (i in 2:length(aftd_mut.inertExpliqueeAbs)) {
#    aftd_mut.inertExpliqueeAbs.cumulated[i] = aftd_mut.inertExpliqueeAbs.cumulated[i] + aftd_mut.inertExpliqueeAbs.cumulated[i-1]
#  }
  total = create_inertie_table(aftd_mut.inertExpliqueeAbs, nbDimensions)
#  rownames(total) = c("Inertie expliquée", "Pourcentage d'inertie expliquée")
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
plot(mut_hclust, hang = -1)
rect.hclust(mut_hclust, k = 2)
rect.hclust(mut_hclust, k = 3)

# max - Distance inter-classes = distance maximum entre leurs objets respectifs les plus distants
# types de classes = circle (by hobby, plot) (cercle de connaissance, de passion commune ?)
mut_hclust = hclust(mut, method = "complete")
plot(mut_hclust, hang = -1)
rect.hclust(mut_hclust, k = 3)

# moyenne - UPGMA - Distance inter-classe = moyenne arithmétique de toutes les distances entre les objets des deux classes  
# souvent méthode par défaut
# type de classe générique
mut_hclust = hclust(mut, method = "average")
plot(mut_hclust, hang = -1)
rect.hclust(mut_hclust, k = 3)

# WPGMA - Simple average, or method of equilibrious between-group average - Même chose que "moyenne" (UPGMA) 
#   sauf que que les sous-classes de la dernière classe ayant fusionée ont une importante égale, indifférement 
#   de leur taille en termes d'effectifs (normalisation par rapport aux effectifs des deux classes qui ont été 
#   regroupées précédemment)
# # type de classe générique
mut_hclust = hclust(mut, method = "mcquitty")
plot(mut_hclust, hang = -1)
rect.hclust(mut_hclust, k = 3)


# UPGMC - Distance inter-classe = distance euclidienne entre leurs centre de gravité respectifs
#   méthode qui n'a pas un indice strictement croissant/décroissant ???
# type de classes = "proximity of platforms (politics)"
mut_hclust = hclust(mut, method = "centroid")
plot(mut_hclust, hang = -1)
rect.hclust(mut_hclust, k = 2)

# WPGMC - "centroid" modifiée - Distance inter-classe = distance entre leur centre de gravité respectifs sauf 
#   que que les sous-classes de la dernière classe ayant fusionée ont une importante égale, indifférement de 
#   leur taille en termes d'effectifs (normalisation par rapport aux effectifs des deux classes qui ont été 
#   regroupées précédemment)
# type de classes = "proximity of platforms (politics)"
#   méthode qui n'a pas un indice strictement croissant/décroissant => perd "monotonie"
mut_hclust = hclust(mut, method = "median")
plot(mut_hclust, hang = -1)
rect.hclust(mut_hclust, k = 2)

# Ward’s method, or minimal increase of sum-of-squares (MISSQ), sometimes incorrectly called "minimum variance" method.
#   Proximity between two clusters is the magnitude by which the summed square in their joint cluster will be 
#   greater than the combined summed square in these two clusters: SS(1,2) − (  SS(1) + SS(2)  ).
#   (Between two singleton objects this quantity = squared euclidean distance / 2.)
# Classe mettant en valeur le "type" des objets de la classe
#   méthode qui n'a pas un indice strictement croissant/décroissant => perd "monotonie"
mut_hclust = hclust(mut, method = "ward.D2")
plot(mut_hclust, hang = -1)
rect.hclust(mut_hclust, k = 2)