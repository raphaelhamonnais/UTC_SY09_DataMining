#1.3 Données Pima
#setwd("E:/study/UTC/GI05/SY09/UTC_SY09_TPs/TP1")
Pima <- read.csv("data/Pima.csv",header = T)
#il y a huit variables: 
summary(Pima)
plot(Pima)
Pima$z <- factor(Pima$z)
dim(Pima)
#supprimer la dernière colonne 
#Pima <- Pima[,-ncol(Pima)]
Pima
#prendre les six premières lignes
#head(Pima)
#summary(Pima)

hist(Pima$npreg)
plot(density(Pima$glu))
#sort(): pour faire l 'ordre des données
plot(sort(Pima$bp))
plot(Pima$ped~Pima$bp)
boxplot(Pima$npreg~Pima$z, names =c("non diabétique", "diabétique"), ylab = "nombre de grossesse(npreg)", xlab = "diabétique ou non(z)", main ="la relation entre npreg et z")
boxplot(Pima$glu~Pima$z, names =c("non diabétique", "diabétique"), ylab = "taux plasmatique de glucose(glu)", xlab = "diabétique ou non(z)", main ="la relation entre glu et z")
boxplot(Pima$bp~Pima$z, names =c("non diabétique", "diabétique"), ylab = "pression artérielle diastolique(bp)", xlab = "diabétique ou non(z)", main ="la relation entre bp et z")
boxplot(Pima$skin~Pima$z, names =c("non diabétique", "diabétique"), ylab = "épaisseur du pli cutané au niveau du triceps(skin)", xlab = "diabétique ou non(z)", main ="la relation entre skin et z")
boxplot(Pima$bmi~Pima$z, names =c("non diabétique", "diabétique"), ylab = "indice de masse corporelle(bmi)", xlab = "diabétique ou non(z)", main ="la relation entre bmi et z")
boxplot(Pima$ped~Pima$z, names =c("non diabétique", "diabétique"), ylab = "fonction de pedigree du diabète(ped)", xlab = "diabétique ou non(z)", main ="la relation entre ped et z")
boxplot(Pima$age~Pima$z, names =c("non diabétique", "diabétique"), ylab = "âge(age)", xlab = "diabétique ou non(z)", main ="la relation entre age et z")

Pima.quant <- Pima[,-ncol(Pima)]
cor(Pima.quant)


a = table(Pima$z[which(Pima$z == 1)], Pima$npreg[which(Pima$z == 1)])
a = as.data.frame(table(Pima$z, Pima$npreg))
a
sum(a[1,])
sum(a[2,])
table(Pima$z, Pima$glu)
plot(Pima$z~Pima$glu)
table(Pima$z, Pima$bp)
table(Pima$z, Pima$skin)
table(Pima$z, Pima$bmi)
table(Pima$z, Pima$ped)
table(Pima$z, Pima$age)



acp = princomp(Pima)
acp
biplot(acp)

acp$scores
plot(acp$scores[,2]~acp$scores[,1], col = c("blue","orange")[Pima$z])
plot(acp$scores[,1]~acp$scores[,7], col = c("blue","orange")[Pima$z])




Pima$z
class(Pima$z)



#########################  New ###################

Pima <- read.csv("data/Pima.csv",header = T)
Pima$z <- factor(Pima$z)
summary(Pima)
contingence_z_npreg = as.data.frame.matrix(table(Pima$z, Pima$npreg))
contingence_z_npreg
for (i in 13:17) {
  contingence_z_npreg[,12] = contingence_z_npreg[,12] + contingence_z_npreg[,i]
}
contingence_z_npreg
contingence_z_npreg = contingence_z_npreg[,-c(13:17)]
contingence_z_npreg
chisq.test(contingence_z_npreg)

