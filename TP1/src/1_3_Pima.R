#1.3 Données Pima
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
boxplot(Pima$npreg~Pima$z, names =c("Non diabétique", "Diabétique"), col = c("light green", "light blue"),ylab = "Nombre de grossesse (npreg)", xlab = "Diabétique ou non (z)", main ="Relation entre diabète et nombre de grossesses")
boxplot(Pima$glu~Pima$z, names =c("Non diabétique", "Diabétique"), col = c("light green", "light blue"),ylab = "Taux plasmatique de glucose (glu)", xlab = "Diabétique ou non (z)", main ="Relation entre diabète et taux de glucose")
boxplot(Pima$bp~Pima$z, names =c("Non diabétique", "Diabétique"), col = c("light green", "light blue"),ylab = "Pression artérielle diastolique (bp)", xlab = "Diabétique ou non (z)", main ="Relation entre diabète et pression artérielle")
boxplot(Pima$skin~Pima$z, names =c("Non diabétique", "Diabétique"), col = c("light green", "light blue"),ylab = "Épaisseur du pli cutané au niveau du triceps (skin)", xlab = "Diabétique ou non (z)", main ="Relation entre diabète et épaisseur pli cutané du triceps")
boxplot(Pima$bmi~Pima$z, names =c("Non diabétique", "Diabétique"), col = c("light green", "light blue"),ylab = "Indice de masse corporelle( bmi)", xlab = "Diabétique ou non (z)", main ="Relation entre diabète et indice de masse corporelle")
boxplot(Pima$ped~Pima$z, names =c("Non diabétique", "Diabétique"), col = c("light green", "light blue"),ylab = "Pedigree génétique du diabète (ped)", xlab = "Diabétique ou non (z)", main ="Relation entre diabète et pédigree génétique")
boxplot(Pima$age~Pima$z, names =c("Non diabétique", "Diabétique"), col = c("light green", "light blue"),ylab = "Age", xlab = "Diabétique ou non (z)", main ="Relation entre diabète et âge")
plot(Pima$npreg~Pima$age)


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
rownames(contingence_z_npreg) = c("Non diabétique", "Diabétique")
colnames(contingence_z_npreg) = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", ">10")
contingence_z_npreg
chisq.test(contingence_z_npreg)
contingence_z_npreg_freq = t(apply(contingence_z_npreg, MARGIN = 1, prop.table)*100)
contingence_z_npreg_freq
barplot(
  as.matrix(contingence_z_npreg_freq), 
  col = c("light green", "light blue"),
  beside = F, 
  legend = row.names(contingence_z_npreg_freq),
  xlab = "Nombre de grossesses", ylab = "Fréquence"
)

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
contingence_z_glu_freq = t(apply(contingence_z_glu, MARGIN = 1, prop.table)*100)
contingence_z_glu_freq
barplot(
  as.matrix(contingence_z_glu_freq), 
  col = c("light green", "light blue"),
  beside = F, 
  legend = row.names(contingence_z_glu_freq),
  xlab = "Taux de glucose", ylab = "Fréquence"
)
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
contingence_z_bp_freq = t(apply(contingence_z_bp, MARGIN = 1, prop.table)*100)
contingence_z_bp_freq
barplot(
  as.matrix(contingence_z_bp_freq), 
  col = c("light green", "light blue"),
  beside = F, 
  legend = row.names(contingence_z_bp_freq),
  xlab = "Pression artérielle", ylab = "Fréquence"
)
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
  beside = F, 
  legend = row.names(contingence_z_skin_freq),
  xlab = "Epaisseur du pli cutané au niveau du triceps", ylab = "Fréquence"
  )
# Conclusion
#  => plis cutané mince => plus de fréquence de non diab / 
#  => plis cutané épais => plus de fréquence de diab / 
#  => plis cutané des diabétiques suit une courbe gaussienne


#Diabète vs. indice de masse corporelle #
#Faire des intervalles 
class_bmi <- cut(Pima$bmi, breaks = c(-Inf,seq(26, 50, 5), Inf), right = FALSE)
class_bmi
#Faire la table de contingence 
contingence_z_bmi = as.data.frame.matrix(table(Pima$z, class_bmi))
#Renommer les lignes de la table de contingence 
row.names(contingence_z_bmi) = c("Non diabétique", "Diabétique")
contingence_z_bmi
#Faire la test chi2
chisq.test(contingence_z_bmi)
#Faire la table en fréquence  
contingence_z_bmi_freq = t(apply(contingence_z_bmi, MARGIN = 1, prop.table)*100)
contingence_z_bmi_freq
#faire la graphe "barplot"
barplot(
  as.matrix(contingence_z_bmi_freq), 
  col = c("light green", "light blue"),
  beside = F, 
  legend = row.names(contingence_z_bmi_freq),
  xlab = "Indice de masse corporelle ", ylab = "Fréquence"
)
#Conclusion:
#Plus indice de masse corporelle est petit, plus de fréquence de non diabétique 
#Plus indice de masse corporelle est grand, plus de fréquence de diabétique 

###Diabète vs.fonction de pedigree du diabète###
#Faire des intervalles 
class_ped <- cut(Pima$ped, breaks = c(seq(0, 1.3, 0.2), Inf), right = FALSE)
class_ped
#Faire la table de contingence 
contingence_z_ped = as.data.frame.matrix(table(Pima$z, class_ped))
#Renommer les lignes de la table de contingence 
row.names(contingence_z_ped) = c("Non diabétique", "Diabétique")
contingence_z_ped
#Faire la test chi2
chisq.test(contingence_z_ped)
#Faire la table en fréquence  
contingence_z_ped_freq = t(apply(contingence_z_ped, MARGIN = 1, prop.table)*100)
contingence_z_ped_freq
#faire la graphe "barplot"
barplot(
  as.matrix(contingence_z_ped_freq), 
  col = c("light green", "light blue"),
  beside = F, 
  legend = row.names(contingence_z_ped_freq),
  xlab = "Fonction de pédigree du diabète", ylab = "Fréquence"
)
#Conclusion:
#Fonction de pedigree non diabétique peut-être suivi une loi de chi2
#Quand la valeur est entre [0.2, 0.4], on a la plus possibilité d'être diabétique et non diabétique  

###Diabète vs. âge ###
#Faire des intervalles  
class_age <- cut(Pima$age, breaks = c(seq(20, 50, 5), Inf), right = FALSE)
class_age
#Faire la table de contingence 
contingence_z_age = as.data.frame.matrix(table(Pima$z, class_age))
#Renommer les lignes de la table de contingence 
row.names(contingence_z_age) = c("Non diabétique", "Diabétique")
contingence_z_age
#Faire la test chi2
chisq.test(contingence_z_age)
#Faire la table en fréquence  
contingence_z_age_freq = t(apply(contingence_z_age, MARGIN = 1, prop.table)*100)
contingence_z_age_freq
#faire la graphe "barplot"
barplot(
  as.matrix(contingence_z_age_freq), 
  col = c("light green", "light blue"),
  beside = F, 
  legend = row.names(contingence_z_age_freq),
  xlab = "Age", ylab = "Fréquence"
)
#Conclusion:
#Plus la personne est jeune, plus la fréquence de non diabétique.
#La fréquence d'être diabétique ne changé pas trop selon l'âge  









## 2.ACP ##################


pima.acp = princomp(pima.quant)
summary(pima.acp)
pima.acp$loadings
pima.acp$scores
correl_pima_acp = cor(pima.quant, pima.acp$scores)
correl_pima_acp
s.corcircle(correl_pima_acp, xax = 1, yax = 2)
s.corcircle(correl_pima_acp, xax = 1, yax = 3)
s.corcircle(correl_pima_acp, xax = 1, yax = 4)
plot(pima.acp$scores[,1], pima.acp$scores[,2],
     col=c("green","blue")[Pima$z],
     xlab="Composante 1", 
     ylab="Composante 2"
)
plot(pima.acp$scores[,1], pima.acp$scores[,3],
     col=c("green","blue")[Pima$z],
     xlab="Composante 1", 
     ylab="Composante 3"
)
plot(pima.acp$scores[,1], pima.acp$scores[,4],
     col=c("green","blue")[Pima$z],
     xlab="Composante 1", 
     ylab="Composante 4"
)

biplot(pima.acp)






# test en changeant poids individus ##############

pima.quant.centered = scale(pima.quant, center = TRUE, scale = TRUE) # centrer les données en colonne
cov.wt(pima.quant.centered, method = 'ML')

Dp = diag(1, length(Pima$z),length(Pima$z))
# calculer la matrice de poid des individus en fonction de leur effectif
nb_non_diab = length(which(Pima$z == "1"))
nb_diab = length(which(Pima$z == "2"))
for (i in 1:length(Pima$z)) {
  if ( i > nb_non_diab ) {
    Dp[i,i] = 1/nb_diab
  }
  else {
    Dp[i,i] = 1/nb_non_diab
  }
}

V = t(pima.quant.centered) %*% Dp %*% pima.quant.centered

pima.acp.dp = princomp(pima.quant, covmat = V)
pima.acp.dp.comp = pima.quant.centered %*% pima.acp.dp$loadings
plot(pima.acp.dp.comp[,1], pima.acp.dp.comp[,5], col=c("green","blue")[Pima$z])
summary(pima.acp.dp)
biplot(pima.acp.dp)
xtable(cor(pima.quant.centered, pima.acp.dp$scores))
s.corcircle(cor(pima.quant.centered, pima.acp.dp$scores), xax = 1, yax = 2)
