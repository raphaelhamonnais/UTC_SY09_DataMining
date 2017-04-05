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
create_inertie_table = function(x) {
  inertExpliquee.cumulated = x
  for (i in 2:length(inertExpliquee.cumulated)) {
    inertExpliquee.cumulated[i] = inertExpliquee.cumulated[i] + inertExpliquee.cumulated[i-1]
  }
  total = rbind(x, inertExpliquee.cumulated)
  rownames(total) = c("Inertie expliquée", "Pourcentage d'inertie expliquée")
  total
}
calc_abs_inertie_explique = function(x, nbDimensions=2) {
  aftd_mut.inertExpliqueeAbs = abs(aftd_mut.valP) / sum(abs(aftd_mut.valP)) * 100
#  aftd_mut.inertExpliqueeAbs.cumulated = aftd_mut.inertExpliqueeAbs
#  for (i in 2:length(aftd_mut.inertExpliqueeAbs)) {
#    aftd_mut.inertExpliqueeAbs.cumulated[i] = aftd_mut.inertExpliqueeAbs.cumulated[i] + aftd_mut.inertExpliqueeAbs.cumulated[i-1]
#  }
  total = create_inertie_table(aftd_mut.inertExpliqueeAbs)
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
  total = create_inertie_table(inertExpliqueeWithoutNegValues)
  total[,1:nbDimensions]
}

##### k = 2 dimensions ######
k = 2
aftd_mut <- cmdscale(mut, k = 2, eig = TRUE, x.ret = TRUE) # eig = avoir les données résultantes de la diagonalisation par eigen x.ret = avoir la matrice doublement centrée W des produits scalaires 
#représentation graphique
aftd_mut$points
plot(aftd_mut$points) # toutes les données
plot(aftd_mut$points[1:17,]) # sans les données extrêmes pour voir s'il y a des classes dans les données regroupées
# Calculer la qualité de la représentation
aftd_mut.valP = aftd_mut$eig
# si valeurs propres négatives : fonction qui calcule l'inertie expliquée avec valeurs absolues des valeurs propres
round(calc_abs_inertie_explique(aftd_mut.valP, k), 2)
# si valeurs propres négatives : fonction qui calcule l'inertie expliquée en ne prenant pas en compte les valeurs propres négatives
round(calc_inertie_explique_witout_neg_values(aftd_mut.valP, k), 2)
# Diagramme de shepard
x = as.dist(aftd_mut$x, diag = T, upper = T) # FIXME pas bon je pense, à refaire
plot(mut, x)

##### k = 3 dimensions ######
k = 3
aftd_mut_3 <- cmdscale(mut, k = 3, eig = TRUE, x.ret = TRUE)
plot(aftd_mut_3$points[,1], aftd_mut_3$points[,3])
plot(aftd_mut_3$points[,2], aftd_mut_3$points[,3])
round(calc_abs_inertie_explique(aftd_mut.valP, k), 2)
round(calc_inertie_explique_witout_neg_values(aftd_mut.valP, k), 2)

##### k = 4 dimensions ######
k = 4
aftd_mut_4 <- cmdscale(mut, k = 4, eig = TRUE, x.ret = TRUE)
plot(aftd_mut_4$points[,1], aftd_mut_4$points[,4])
plot(aftd_mut_4$points[,2], aftd_mut_4$points[,4])
plot(aftd_mut_4$points[,3], aftd_mut_4$points[,4])
round(calc_abs_inertie_explique(aftd_mut.valP, k), 2)
round(calc_inertie_explique_witout_neg_values(aftd_mut.valP, k), 2)

##### k = 5 dimensions ######
k = 5
aftd_mut_5 <- cmdscale(mut, k = 4, eig = TRUE, x.ret = TRUE)
plot(aftd_mut_5$points[,1], aftd_mut_5$points[,5])
plot(aftd_mut_5$points[,2], aftd_mut_5$points[,5])
plot(aftd_mut_5$points[,3], aftd_mut_5$points[,5])
plot(aftd_mut_5$points[,4], aftd_mut_5$points[,5])
round(calc_abs_inertie_explique(aftd_mut.valP, k), 2)
round(calc_inertie_explique_witout_neg_values(aftd_mut.valP, k), 2)









##########






