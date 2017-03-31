#1.3 Données Pima
#setwd("E:/study/UTC/GI05/SY09/UTC_SY09_TPs/TP1")
Pima <- read.csv("data/Pima.csv",header = T)
Pima$z <- factor(Pima$z)
#il y a huit variables: 
summary(Pima)
plot(Pima)

dim(Pima)


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



#### Corrélation pima quantitatif ######
pima.quant <- Pima[,-ncol(Pima)]
cor(pima.quant)

#### Etude indépendance variables avec test khi 2 #########

## Diabète vs. Grossesses #########
contingence_z_npreg = as.data.frame.matrix(table(Pima$z, Pima$npreg))
contingence_z_npreg
for (i in 13:17) {
  contingence_z_npreg[,12] = contingence_z_npreg[,12] + contingence_z_npreg[,i]
}
contingence_z_npreg
contingence_z_npreg = contingence_z_npreg[,-c(13:17)]
contingence_z_npreg
chisq.test(contingence_z_npreg)
# Conclusion => enceinte = augmente risques de diabète. L'autre solution envisageable est que le diabète augmente la fertilité des femmes, mais cela semble moins logique



#diabète vs. indice #
class_bmi <- cut(Pima$bmi, breaks = c(-Inf,seq(26, 50, 5), Inf))
class_bmi
contingence_z_bmi = as.data.frame.matrix(table(Pima$z, class_bmi))
contingence_z_bmi
chisq.test(contingence_z_bmi)
#conclusion:

#diabete vs. 
Pima$ped
hist(Pima$ped)
class_ped <- cut(Pima$ped, breaks = c(seq(0, 1.3, 0.2), Inf))
class_ped
contingence_z_ped = as.data.frame.matrix(table(Pima$z, class_ped))
contingence_z_ped
chisq.test(contingence_z_ped)
#conclusion:

####diabète vs. âge ###
#Faire des intervalles  
class_age <- cut(Pima$age, breaks = c(seq(20, 50, 5), Inf))
class_age
#Faire la table de contingence 
contingence_z_age = as.data.frame.matrix(table(Pima$z, class_age))
contingence_z_age
#Faire la test chi2
chisq.test(contingence_z_age)
#barplot(as.matrix(contingence_z_age), beside = T)

#conclusion: 

