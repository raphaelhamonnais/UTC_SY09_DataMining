crabs2 <- read.csv("data/crabs2.csv", na.strings="", header=T)
crabs2
summary(crabs2)
#supprimer les deux dernieres lignes
crabs2_quant <- crabs2[,1:4]
crabs2_quant
summary(crabs2_quant)
dim(crabs2_quant)
d1 <- dim(crabs2_quant)[1]
d1
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
#3.1 Effectuer plusieurs classifications en K = 2 classes
#selon l'espèce 
for (i in 1:5)
{
  cat('Classifiction',i,'\n')
  crabs2_kmeans_2 <- kmeans(crabs2_quant,2)
  print(crabs2_kmeans_2$cluster)
  cat('L\'inertie intra-classe:',crabs2_kmeans_2$tot.withinss/d1,'\n')
  plot(crabs2_quant, col = crabs2_kmeans_2$cluster, pch = as.integer(crabs2$sp))
  Sys.sleep(2)
}

#selon le sex 
for (i in 1:5)
{
  cat('Classifiction',i,'\n')
  crabs2_kmeans_2 <- kmeans(crabs2_quant,2)
  print(crabs2_kmeans_2$cluster)
  cat('L\'inertie intra-classe:',crabs2_kmeans_2$tot.withinss/d1,'\n')
  plot(crabs2_quant, col = crabs2_kmeans_2$cluster, pch = as.integer(crabs2$sex))
  Sys.sleep(2)
}

#3.2 Effectuer plusieurs classifications en K = 4 classes
#selon l'espèce 
for (i in 1:5)
{
  cat('Classifiction',i,'\n')
  crabs2_kmeans_4 <- kmeans(crabs2_quant,4)
  print(crabs2_kmeans_4$cluster)
  cat('L\'inertie intra-classe:',crabs2_kmeans_4$tot.withinss/d1,'\n')
  plot(crabs2_quant, col = crabs2_kmeans_4$cluster, pch = as.integer(crabs2$sp))
  Sys.sleep(2)
}

#selon le sex 
for (i in 1:5)
{
  cat('Classifiction',i,'\n')
  crabs2_kmeans_4 <- kmeans(crabs2_quant,4)
  print(crabs2_kmeans_4$cluster)
  cat('L\'inertie intra-classe:',crabs2_kmeans_4$tot.withinss/d1,'\n')
  plot(crabs2_quant, col = crabs2_kmeans_4$cluster, pch = as.integer(crabs2$sex))
  Sys.sleep(2)
}
