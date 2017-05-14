#Charger les données Crabs
library(plyr)
crabs2 <- read.csv("data/crabs2.csv", na.strings="", header=T)
crabs2
summary(crabs2)
#supprimer les deux dernieres lignes "sp" et "sex"
crabs2_quant <- crabs2[,1:4]
crabs2_quant
summary(crabs2_quant)
dim(crabs2_quant)
#Obtenir le nombre de ligne des données 
d1 <- dim(crabs2_quant)[1]
d1

######## 1 - affichage dans le premier plan factoriel #########
#faire l'acp
acp_crabs2_quant <- princomp(crabs2_quant)
acp_crabs2_quant
#affiche le premier plan factoriel sans tenir compte de l'espece
plot(acp_crabs2_quant$scores, asp = 1)
##affiche le premier plan factoriel en tenant compte de l'espece
plot(acp_crabs2_quant$scores[,1:2], col=c("blue","orange")[crabs2$sp], pch=c(21,24)[crabs2$sex], asp = 1)
legend(0.064, 0.067, legend=c("Bleu", "Orange"), col=c("blue", "orange"), pch=16, cex=.8)
legend(0.064, 0.050, legend=c("Femelle", "Mâle"), pch=c(21,24), cex=.8)


#################### 3.méthode des centres mobiles #################################

species_colors = mapvalues(crabs2$sp, from = c("B", "O"), to = c("blue", "darkorange"))
sex_colors = mapvalues(crabs2$sex, from = c("M", "F"), to = c("red", "darkgreen"))


#3.1 Effectuer plusieurs classifications en K = 2 classes
nbComparaisons=1000
classifications_seen = vector(length = nbComparaisons)
for (i in 1:nbComparaisons) {
  crabs2_kmeans_2 <- kmeans(crabs2_quant,2)
  classifications_seen[i] = crabs2_kmeans_2$tot.withinss
}
unique(classifications_seen)
t(t(table(round(classifications_seen,5))/1000*100))
xtable(t(t(table(round(classifications_seen,5))/1000*100)))

getExpectedInertie = function(tabQuant, expectedInertie, nbClasses, roundN) {
  kmeansResult <- kmeans(tabQuant, nbClasses)
  while (round(kmeansResult$tot.withinss, roundN) != expectedInertie) {
    kmeansResult <- kmeans(tabQuant, nbClasses)
  }
  kmeansResult
}

crabs2_kmeans_2 = getExpectedInertie(crabs2_quant, expectedInertie = 0.3648, 2, 5)
crabs2_kmeans_2 = getExpectedInertie(crabs2_quant, expectedInertie = 0.358, 2, 5)
crabs2_kmeans_2 = getExpectedInertie(crabs2_quant, expectedInertie = 0.35754, 2, 5)
crabs2_kmeans_2 = getExpectedInertie(crabs2_quant, expectedInertie = 0.35607, 2, 5)
crabs2_kmeans_2 = getExpectedInertie(crabs2_quant, expectedInertie = 0.25878, 2, 5)
clusplot(crabs2_quant, crabs2_kmeans_2$cluster, color = TRUE, col.p = as.character(species_colors), lines = F, main = "", ylab = "Sexe", xlab = "Espèce")
legend(2,2.2, legend=c("Espèce Bleu", "Espèce Orange"), col=c("blue", "darkorange"), pch=16, cex=.8)
clusplot(crabs2_quant, crabs2_kmeans_2$cluster, color = TRUE, col.p = as.character(sex_colors),lines = F, main = "", ylab = "Sexe", xlab = "Espèce")
legend(2,2.2, legend=c("Mâle", "Femelle"), col=c("red", "darkgreen"), pch=16, cex=.8)

#3.2 Effectuer une classifications en K = 4 classes
#méthode un 
#selon l'espèce
min_inertie = 10000
best_crabs_kmeans_4 <- kmeans(crabs2_quant,4)
for (i in 1:100) {
  crabs2_kmeans_4 <- kmeans(crabs2_quant,4)
  if (crabs2_kmeans_4$tot.withinss < min_inertie) {
    min_inertie = crabs2_kmeans_4$tot.withinss
    best_crabs_kmeans_4 = crabs2_kmeans_4
  }
}
best_crabs_kmeans_4$tot.withinss
clusplot(crabs2_quant, best_crabs_kmeans_4$cluster, color = TRUE, col.p = as.character(species_colors), lines = F, main = "Comparaison K-Means 4 classes et Espèce", ylab = "Sexe", xlab = "Espèce")
legend(2,2.2, legend=c("Espèce Bleu", "Espèce Orange"), col=c("blue", "darkorange"), pch=16, cex=.8)
clusplot(crabs2_quant, best_crabs_kmeans_4$cluster, color = TRUE, col.p = as.character(sex_colors),lines = F, main = "Comparaison K-Means 4 classes et Sexe", ylab = "Sexe", xlab = "Espèce")
legend(2,2.2, legend=c("Mâle", "Femelle"), col=c("red", "darkgreen"), pch=16, cex=.8)

