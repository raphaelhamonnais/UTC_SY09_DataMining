crabs2 <- read.csv("data/crabs2.csv", na.strings="", header=T)
crabs2
summary(crabs2)
#supprimer les deux dernieres lignes
crabs2_quant <- crabs2[,1:4]
crabs2_quant
summary(crabs2_quant)
dim(crabs2_quant)
#faire l'acp
acp_crabs2_quant <- princomp(crabs2_quant)
acp_crabs2_quant
#affiche le premier plan factoriel sans tenir compte de l'espece
plot(acp_crabs2_quant$scores)
##affiche le premier plan factoriel en tenant compte de l'espece
plot(acp_crabs2_quant$scores[,1:2], col=c("blue","orange")[crabs2$sp], pch=c(21,24)[crabs2$sex])
