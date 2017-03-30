#1.2 donnees crabs
#installer le package "MASS" qui contient la data "crabs" 
install.packages("MASS")
#charger le package "MASS"
library(MASS)


#morphométrique du crabe:
#   - 200 lignes et 8 colonnes
#   - 4 groupes(chaque groupe a 50 crabes)
#   - cinq morphométriques
#     - FL: taille de la frontale
#     - RW: largeur de la queue
#     - CL: longueur de la coquille
#     - CW: largeur de la coque
#     - BD: la profondeur du corps
#   - deux espèces différentes selon la couleur (B: bleu, O: orange)
#   - le sexe (male / femelle)

#transférer des données au fichier .csv 
#write.csv(crabs, file = "E:/study/UTC/GI05/SY09/TD/TD1/crabs.csv")
#voir la définition et des détails des données "crabs"
help(crabs)

#faire un sommaire 
summary(crabs)
crabs$sp <- factor(crabs$sp, levels = c("B","O"))
crabs$sex <- factor(crabs$sex, levels = c("F","M"))
#dessiner les données en utilisant les fonctions plot() et boxplot()
plot(crabs)
boxplot(crabs)

#data(): charger l'ensemble de données spécifiées ou lister l'ensemble de données disponibles
data(crabs)

#cinq morphométriques du crabe: FL, RW, CL, CW, BD
crabsquant <- crabs[,4:8]
crabsquant
summary(crabsquant)
plot(crabsquant)
boxplot(crabsquant)

# Obtenir les index des crabes selon leur sexe : male et femelle  
Male_Indexes <- which(crabs$sex == "M")
Male_Indexes
Female_Indexes <- which(crabs$sex == "F")
Female_Indexes

# Obtenir les index des crabes selon leur espèce : bleu ou orange
Blue_Indexes  <- which(crabs$sp == "B")
Blue_Indexes
Orange_Indexes <- which(crabs$sp == "O")
Orange_Indexes

#1.2.1
#différencier graphiquement les données en fonction de l'espèce
crabs$sp
plot(crabsquant,col = c("blue","orange")[crabs$sp]) # => aucune différenciation visible
dev.off()
#différencier graphiquement les données en fonction du sexe
plot(crabsquant,col=c("black","red")[crabs$sex]) # => aucune différenciation visible
dev.off()



#Etudier selon l'espèce
Crabs_Orange <- crabsquant[Orange_Indexes,]
Crabs_Orange
Crabs_Blue <- crabsquant[Blue_Indexes,]
Crabs_Blue

#crabes males, (index 1-50 et 101-150)
Crabs_Male <- crabsquant[Male_Indexes,]
Crabs_Male
#crabes males appartenant à l'espèce de couleur bleue, (index 1-50)
Crabs_Male_and_Blue <- Crabs_Male[Male_Indexes == Blue_Indexes,]
Crabs_Male_and_Blue
#crabes males appartenant à l'espèce de couleur orange, (index 101-150)
Crabs_Male_and_Orange <- Crabs_Male[! Male_Indexes == Blue_Indexes,]
Crabs_Male_and_Orange

#comparer le paramètre FL en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Blue$FL, Crabs_Orange$FL, col = c("light blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Comparaison de la variable FL selon l'espèce")
#comparer le paramètre RW en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Blue$RW, Crabs_Orange$RW, col = c("light blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Comparaison de la variable RW selon l'espèce")
#comparer le paramètre CL en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Blue$CL, Crabs_Orange$CL, col = c("light blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Comparaison de la variable CL selon l'espèce")
#comparer le paramètre CW en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Blue$CW, Crabs_Orange$CW, col = c("light blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Comparaison de la variable CW selon l'espèce")
#comparer le paramètre BD en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Blue$BD, Crabs_Orange$BD, col = c("light blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Comparaison de la variable BD selon l'espèce")

#comparer le paramètre FL de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$FL, Crabs_Male_and_Orange$FL, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Males - Comparaison de la variable FL selon l'espèce")
#comparer le paramètre RW de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$RW, Crabs_Male_and_Orange$RW, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Males - Comparaison de la variable RW selon l'espèce")
#comparer le paramètre CL de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$CL, Crabs_Male_and_Orange$CL, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Males - Comparaison de la variable CL selon l'espèce")
#comparer le paramètre CW de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$CW, Crabs_Male_and_Orange$CW, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Males - Comparaison de la variable CW selon l'espèce")
#comparer le paramètre BD de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$BD, Crabs_Male_and_Orange$BD, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Males - Comparaison de la variable BD selon l'espèce")

#crabes femelles (index 51-100 et 151-200)
Crabs_Female <- crabsquant[Female_Indexes,]
Crabs_Female
#crabes femelles appartenant à l'espèce de couleur bleue (index 51-100)
Crabs_Female_and_Blue <- Crabs_Female[!Female_Indexes == Orange_Indexes,]
Crabs_Female_and_Blue
#crabes femelles appartenant à l'espèce de couleur orange (index 151-200)
Crabs_Female_and_Orange <- Crabs_Female[Female_Indexes == Orange_Indexes,]
Crabs_Female_and_Orange
#comparer le paramètre FL de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$FL, Crabs_Female_and_Orange$FL, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Femelles - Comparaison de la variable FL selon l'espèce")
#comparer le paramètre RW de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$RW, Crabs_Female_and_Orange$RW, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Femelles - Comparaison de la variable RW selon l'espèce")
#comparer le paramètre CL de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$CL, Crabs_Female_and_Orange$CL, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Femelles - Comparaison de la variable CL selon l'espèce")
#comparer le paramètre CW de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$CW, Crabs_Female_and_Orange$CW, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Femelles - Comparaison de la variable CW selon l'espèce")
#comparer le paramètre BD de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$BD, Crabs_Female_and_Orange$BD, col = c("blue","orange"), names = c("Esp. Bleue","Esp. Orange"), main = "Femelles - Comparaison de la variable BD selon l'espèce")


#etudier selon le sexe

#comparer le paramètre FL des crabes (toutes espèces confondues) en fonction du sexe (male/femelle)
boxplot(Crabs_Male$FL, Crabs_Female$FL, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Comparaison de la variable FL selon le sexe")
#comparer le paramètre FL des crabes (toutes espèces confondues) en fonction du sexe (male/femelle)
boxplot(Crabs_Male$RW, Crabs_Female$RW, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Comparaison de la variable RW selon le sexe")
#comparer le paramètre FL des crabes (toutes espèces confondues) en fonction du sexe (male/femelle)
boxplot(Crabs_Male$CL, Crabs_Female$CL, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Comparaison de la variable CL selon le sexe")
#comparer le paramètre FL des crabes (toutes espèces confondues) en fonction du sexe (male/femelle)
boxplot(Crabs_Male$CW, Crabs_Female$CW, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Comparaison de la variable CW selon le sexe")
#comparer le paramètre FL des crabes (toutes espèces confondues) en fonction du sexe (male/femelle)
boxplot(Crabs_Male$BD, Crabs_Female$BD, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Comparaison de la variable BD selon le sexe")

#comparer le paramètre FL des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$FL, Crabs_Female_and_Blue$FL, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Bleue - Comparaison de la variable FL selon le sexe")
#comparer le paramètre RW des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$RW, Crabs_Female_and_Blue$RW, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Bleue - Comparaison de la variable RW selon le sexe")
#comparer le paramètre CL des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$CL, Crabs_Female_and_Blue$CL, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Bleue - Comparaison de la variable CL selon le sexe")
#comparer le paramètre CW des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$CW, Crabs_Female_and_Blue$CW, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Bleue - Comparaison de la variable CW selon le sexe")
#comparer le paramètre BD des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$BD, Crabs_Female_and_Blue$BD, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Bleue - Comparaison de la variable BD selon le sexe")

#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$FL, Crabs_Female_and_Orange$FL, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Orange - Comparaison de la variable FL selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$RW, Crabs_Female_and_Orange$RW, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Orange - Comparaison de la variable RW selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$CL, Crabs_Female_and_Orange$CL, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Orange - Comparaison de la variable CL selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$CW, Crabs_Female_and_Orange$CW, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Orange - Comparaison de la variable CW selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$BD, Crabs_Female_and_Orange$BD, col = c("grey","white"), names = c("Mâle","Femelle"), main = "Espèce Orange - Comparaison de la variable BD selon le sexe")




#1.2.2
#corrélation 
#selon le résultat, on voit qu'il existe une forte corrélation entre des variables car la corrélation entre deux différentes variables est presque égale à 1.
cor(crabsquant)
xtable(cor(crabsquant))
# on remarque dans les boxplot que la variable RW semble la plus discriminante et effectivement, c'est celle qui possède la corrélation la plus "faible", si tant est que 0,9 puisse être une corrélation dite "faible"
cor(Crabs_Orange)
cor(Crabs_Blue)
cor(Crabs_Blue, Crabs_Orange)
cor(Crabs_Male, Crabs_Female) # set le moins corrélé
cor(Crabs_Male_and_Blue, Crabs_Female_and_Blue)
cor(Crabs_Male_and_Blue, Crabs_Female_and_Orange)
cor(Crabs_Male_and_Orange, Crabs_Female_and_Orange)
cor(Crabs_Male_and_Orange, Crabs_Female_and_Blue)

# Question : Quelle en est vraisemblablement la cause? 

# réponse = effet taille (carac morphologique, indicateurs pays corrélés à leur taille/population, ....)

# Quel traitement est-il possible d’appliquer aux données pour s’affranchir de ce phénomène ?
# ACP => cf. cours



############# 2.3 ACP sur crabs   ##########


###### Fait à la main ###########
crabsquant
crabsquant.centered = scale(crabsquant, center = TRUE, scale = FALSE) # centrer les données en colonne
crabsquant.centered
crabs.covar = cov.wt(crabsquant, method = "ML")
crabs.covar
crabs.covar.diago = eigen(crabs.covar$cov) # diagonaliser la covariance (pas les moyennes)
crabs.covar.diago
crabs.acp.valeurs.propres = crabs.covar.diago$values
crabs.acp.valeurs.propres
crabs.acp.vecteurs.propres = crabs.covar.diago$vectors
crabs.acp.vecteurs.propres
pourcentage_inertie = crabs.acp.valeurs.propres / sum(crabs.acp.valeurs.propres) * 100
pourcentage_inertie
crabs.acp = crabsquant.centered %*% crabs.acp.vecteurs.propres
crabs.acp



# Que constatez vous ? 
pourcentage_inertie # composante 1 donne 98,25 % de l'inertie
# effet de taille toujours, ce qui explique le plus les différences entre les individus de l'échantillon est leur taille. 
# Si l'individu est gros, ses mesures seront importantes, s'il est  plus petit, ses mesures vont diminuer aussi


plot(crabs.acp[,1], col = c("blue","orange")[crabs$sp]) # juste ACP 1 ne dit pas grand chose

# Détermination sexe avec ACP
# y = ACP 1
plot(crabs.acp[,1]~crabs.acp[,3], col = c("black","red")[crabs$sex]) # 1 et 3 moyen pour sexe
plot(crabs.acp[,1]~crabs.acp[,2], col = c("black","red")[crabs$sex]) # bien pour sexe
# x = ACP 1
plot(crabs.acp[,2]~crabs.acp[,1], col = c("black","red")[crabs$sex]) # bien pour sexe
plot(crabs.acp[,3]~crabs.acp[,1], col = c("black","red")[crabs$sex]) # 1 et 3 moyen pour sexe

# Détermination espèce avec ACP
# x = ACP 1
plot(crabs.acp[,2]~crabs.acp[,1], col = c("blue","orange")[crabs$sp]) # moyen pour espèce
plot(crabs.acp[,3]~crabs.acp[,1], col = c("blue","orange")[crabs$sp]) # 1 et 3 bien pour espèce
# y = ACP 1
plot(crabs.acp[,1]~crabs.acp[,2], col = c("blue","orange")[crabs$sp]) # moyen pour espèce
plot(crabs.acp[,1]~crabs.acp[,3], col = c("blue","orange")[crabs$sp]) # 1 et 3 bien pour espèce

# Autres composantes ne disent pas grand chose
plot(crabs.acp[,1]~crabs.acp[,4], col = c("blue","orange")[crabs$sp]) # ne dit rien d'intéressant mais c'est normal vu le pourcentage d'inertie expliquée
plot(crabs.acp[,1]~crabs.acp[,4], col = c("black","red")[crabs$sex]) # ne dit rien d'intéressant mais c'est normal vu le pourcentage d'inertie expliquée
plot(crabs.acp[,1]~crabs.acp[,5], col = c("blue","orange")[crabs$sp]) # ne dit rien d'intéressant mais c'est normal vu le pourcentage d'inertie expliquée
plot(crabs.acp[,1]~crabs.acp[,5], col = c("black","red")[crabs$sex]) # ne dit rien d'intéressant mais c'est normal vu le pourcentage d'inertie expliquée




###### Fait avec les fonctions de R ##########

acp_sur_crabes = princomp(crabsquant)


# Que constatez vous ? 
options(digits = 4)
summary(acp_sur_crabes) # composante 1 donne 98,25 % de l'inertie
# effet de taille toujours, ce qui explique le plus les différences entre les individus de l'échantillon est leur taille. 
# Si l'individu est gros, ses mesures seront importantes, s'il est  plus petit, ses mesures vont diminuer aussi
acp_sur_crabes$scores
acp_sur_crabes$loadings
cor(crabsquant, acp_sur_crabes$scores)
xtable(cor(crabsquant, acp_sur_crabes$scores), digits = 3)
biplot(acp_sur_crabes) # toutes les variables d'origine sont corrélées positivement et s'expriment presque à 100% sur la composante 1
biplot(acp_sur_crabes, choices = c(2,3))


# Trouver une solution pour améliorer affichage graphique

# Pondérer une ou plusieur des variables responsables de l'effet de taille
# Solution = pour chaque individu i, exprimer l'ensemble
# des variables en poucentages de la valeur de l'observation CL
# CL vaudra alors toujours 100% et n'aura plus une aussi grande dispersion qu'avant, dispersion qui en faisait une variable importante pour l'ACP
# de même, CW qui semble être l'autre variable induisant un effet de taille possède des valeurs très proches de CL : sa dispersion sera elle aussi réduite
# on va donc pouvoir analyser les données et dégager des composantes principales caractérisant autre chose que la taille, par exemple l'espèce et le sexe si c'est réellement des caractères qui influent sur les données morphométriques

# enlever effet taille avec % par rapport à crabsquant$CL
#crabsquant_norm_taille = crabsquant / ((crabsquant$CW+crabsquant$CL)/2) * 100
crabsquant_norm_taille = crabsquant / (crabsquant$CW) * 100
crabsquant_norm_taille = crabsquant / (crabsquant$CL) * 100
# Comparer les valeurs avant et après correction pour le premier individu
comparatif = rbind(crabsquant[1,], crabsquant_norm_taille[1,])
row.names(comparatif) = c("Avant correction", "Après correction")
comparatif
xtable(comparatif)
# ACP sur les nouvelles observations
acp_moins_taille = princomp(crabsquant_norm_taille)
# Correlation de la nouvelle ACP avec les variables d'origine
summary(acp_moins_taille)
cor(crabsquant, acp_moins_taille$scores)
biplot(acp_moins_taille)


library(rgl)
blue = which(crabs$sp == "B")
orange = which(! crabs$sp == "B")
orange
blue
plot3d(acp_moins_taille$scores[,1:3], col = which(crabs$sex=="M" & crabs$sp=="O"))

