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
#crabs:з????̬??��: ??200?к?8??, 4??ÿ??50ֻ??з??5????̬??��(FL: ??Ҷ?ߴ?, RW:β??????, CL:?ǳ?, CW:?ǿ?, BD:????????), ��????ɫ(B:��ɫ, O:??ɫ), ?Ա?

#transférer des données au fichier .csv 
#write.csv(crabs, file = "E:/study/UTC/GI05/SY09/TD/TD1/crabs.csv")
#voir la définition et des détails des données "crabs"
help(crabs)

#faire un sommaire 
summary(crabs)

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
Male <- which(crabs$sex == "M")
Male
Female <- which(crabs$sex == "F")
Female

# Obtenir les index des crabes selon leur espèce : bleu ou orange
Blue  <- which(crabs$sp == "B")
Blue
Orange <- which(crabs$sp == "O")
Orange

#1.2.1
#différencier graphiquement les données en fonction de l'espèce 
plot(crabsquant,col = c("blue","orange")[crabs$sp]) # => aucune différenciation visible
dev.off()
#différencier graphiquement les données en fonction du sexe
plot(crabsquant,col=c("black","red")[crabs$sex]) # => aucune différenciation visible
dev.off()

#Etudier selon l'espèce 
#crabes males, (index 1-50 et 101-150)
CM <- crabsquant[Male,]
CM
#crabes males appartenant à l'espèce de couleur bleue, (index 1-50)
CMB <- CM[Male == Blue,]
CMB
#crabes males appartenant à l'espèce de couleur orange, (index 101-150)
CMO <- CM[!Male == Blue,]
CMO
#comparer le paramètre FL de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(CMB$FL,CMO$FL,col = c("blue","orange"), main = "Males - Comparaison de la variable FL selon l'espèce")
#comparer le paramètre RW de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(CMB$RW,CMO$RW,col = c("blue","orange"), main = "Males - Comparaison de la variable RW selon l'espèce")
#comparer le paramètre CL de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(CMB$CL,CMO$CL,col = c("blue","orange"), main = "Males - Comparaison de la variable CL selon l'espèce")
#comparer le paramètre CW de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(CMB$CW,CMO$CW,col = c("blue","orange"), main = "Males - Comparaison de la variable CW selon l'espèce")
#comparer le paramètre BD de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(CMB$BD,CMO$BD,col = c("blue","orange"), main = "Males - Comparaison de la variable BD selon l'espèce")

#crabes femelles (index 51-100 et 151-200)
CF <- crabsquant[Female,]
CF
#crabes femelles appartenant à l'espèce de couleur bleue (index 51-100)
CFB <- CF[!Female == Orange,]
CFB
#crabes femelles appartenant à l'espèce de couleur orange (index 151-200)
CFO <- CF[Female == Orange,]
CFO
#comparer le paramètre FL de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(CFB$FL,CFO$FL,col = c("blue","orange"), main = "Femelles - Comparaison de la variable FL selon l'espèce")
#comparer le paramètre RW de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(CFB$RW,CFO$RW,col = c("blue","orange"), main = "Femelles - Comparaison de la variable RW selon l'espèce")
#comparer le paramètre CL de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(CFB$CL,CFO$CL,col = c("blue","orange"), main = "Femelles - Comparaison de la variable CL selon l'espèce")
#comparer le paramètre CW de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(CFB$CW,CFO$CW,col = c("blue","orange"), main = "Femelles - Comparaison de la variable CW selon l'espèce")
#comparer le paramètre BD de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(CFB$BD,CFO$BD,col = c("blue","orange"), main = "Femelles - Comparaison de la variable BD selon l'espèce")


#etudier selon le sexe
#crabes de couleur bleue (index: 1-100)
CB <- crabsquant[Blue,]
CB
#crabes males de couleur bleue (index 1-50)
CBM <- na.omit(CB[Male,])
CBM
#crabes femelles de couleur bleue (index 51-100)
CBF <- na.omit(CB[Female,])
CBF
#comparer le paramètre FL des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(CBM$FL,CBF$FL,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable FL selon le sexe")
#comparer le paramètre RW des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(CBM$RW,CBF$RW,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable RW selon le sexe")
#comparer le paramètre CL des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(CBM$CL,CBF$CL,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable CL selon le sexe")
#comparer le paramètre CW des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(CBM$CW,CBF$CW,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable CW selon le sexe")
#comparer le paramètre BD des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(CBM$BD,CBF$BD,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable BD selon le sexe")

#crabes de couleur orange (index 101-200)
CO <- crabsquant[Orange,]
CO
#crabes males de couleur bleue (index 1-50)
COM <- na.omit(CO[Male,])
COM
#crabes femelles de couleur bleue (index 51-100)
COF <- na.omit(CO[Female,])
COF
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(COM$FL,COF$FL,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable FL selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(COM$RW,COF$RW,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable RW selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(COM$CL,COF$CL,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable CL selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(COM$CW,COF$CW,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable CW selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(COM$BD,COF$BD,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable BD selon le sexe")


#1.2.2
#corrélation 
#selon le résultat, on voit qu'il existe une forte corrélation entre des variables car la corrélation entre deux différentes variables est presque égale à 1.
cor(crabsquant)