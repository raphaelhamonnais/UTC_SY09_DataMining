library(xtable)


######## Initialisation données ##########

mut <- read.csv("data/mutations2.csv", header=T, row.names = 1)
mut
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

##### k = 3 dimensions
aftd_mut_3 <- cmdscale(mut, k = 3, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_mut_3$points, diag = T, upper = T), xlab = "Dissimilarités originelles", ylab = "Dissimilarités espace factoriel à 3 dimensions")

##### k = 4 dimensions ######
aftd_mut_4 <- cmdscale(mut, k = 4, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_mut_4$points, diag = T, upper = T), xlab = "Dissimilarités originelles", ylab = "Dissimilarités espace factoriel à 4 dimensions")

##### k = 5 dimensions ######
aftd_mut_5 <- cmdscale(mut, k = 5, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_mut_5$points, diag = T, upper = T), xlab = "Dissimilarités originelles", ylab = "Dissimilarités espace factoriel à 5 dimensions")







