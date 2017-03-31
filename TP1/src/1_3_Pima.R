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






## Diabète vs. Glucose #########
hist(Pima$glu)
summary(Pima$glu)
classes_glu = cut(Pima$glu, 
        #breaks = c(-Inf, 90, 100, 110, 120, 130, 140, 150, 160, 175, Inf), 
        breaks = c(-Inf, seq(90,160,10), 175, Inf), 
#        labels = c("0-89", "90-99", "d", "100-119", "ed", "120-139", "140-159", "", ">= 160", "de"), 
        right = FALSE) # regrouper en classes
classes_glu
contingence_z_glu = as.data.frame.matrix(table(Pima$z, classes_glu))
contingence_z_glu
row.names(contingence_z_glu) = c("Non diabétique", "Diabétique")
contingence_z_glu
chisq.test(contingence_z_glu)
# Conclusion => taux de glucose élevé = diabétique



## Diabète vs. BP pression artérielle #########
hist(Pima$bp)
summary(Pima$bp)
classes_bp = cut(Pima$bp,
                  breaks = c(-Inf, seq(51,100,10), Inf), 
                  right = FALSE) # regrouper en classes
classes_bp
contingence_z_bp = as.data.frame.matrix(table(Pima$z, classes_bp))
contingence_z_bp
row.names(contingence_z_bp) = c("Non diabétique", "Diabétique")
contingence_z_bp
chisq.test(contingence_z_bp)
# Conclusion => taux de glucose élevé = effet sur pression artérielle


## Diabète vs. épaisseur du pli cutané au niveau du triceps #########
hist(Pima$skin)
summary(Pima$skin)
classes_skin = cut(Pima$skin,
                 breaks = c(-Inf, 15, seq(20,45,5), 49, Inf), 
                 right = FALSE) # regrouper en classes
classes_skin
contingence_z_skin = as.data.frame.matrix(table(Pima$z, classes_skin))
contingence_z_skin
row.names(contingence_z_skin) = c("Non diabétique", "Diabétique")
contingence_z_skin
chisq.test(contingence_z_skin)
contingence_z_skin_freq = t(apply(contingence_z_skin, MARGIN = 1, prop.table)*100)
contingence_z_skin_freq
barplot(
  as.matrix(contingence_z_skin_freq), 
  col = c("light green", "light blue"),
  beside = T, 
  legend = row.names(contingence_z_skin_freq),
  xlab = "Epaisseur du pli cutané au niveau du triceps", ylab = "Fréquence"
  )
# Conclusion
#  => plis cutané mince => plus de fréquence de non diab / 
#  => plis cutané épais => plus de fréquence de diab / 
#  => plis cutané des diabétiques suit une courbe gaussienne





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