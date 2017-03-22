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
#crabes males, (index 1-50 et 101-150)
Crabs_Male <- crabsquant[Male_Indexes,]
Crabs_Male
#crabes males appartenant à l'espèce de couleur bleue, (index 1-50)
Crabs_Male_and_Blue <- Crabs_Male[Male_Indexes == Blue_Indexes,]
Crabs_Male_and_Blue
#crabes males appartenant à l'espèce de couleur orange, (index 101-150)
Crabs_Male_and_Orange <- Crabs_Male[! Male_Indexes == Blue_Indexes,] # TODO [Male_Indexes == Orange_Indexes] ne marche pas, pourquoi ???
Crabs_Male_and_Orange
#comparer le paramètre FL de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$FL,Crabs_Male_and_Orange$FL,col = c("blue","orange"), main = "Males - Comparaison de la variable FL selon l'espèce")
#comparer le paramètre RW de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$RW,Crabs_Male_and_Orange$RW,col = c("blue","orange"), main = "Males - Comparaison de la variable RW selon l'espèce")
#comparer le paramètre CL de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$CL,Crabs_Male_and_Orange$CL,col = c("blue","orange"), main = "Males - Comparaison de la variable CL selon l'espèce")
#comparer le paramètre CW de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$CW,Crabs_Male_and_Orange$CW,col = c("blue","orange"), main = "Males - Comparaison de la variable CW selon l'espèce")
#comparer le paramètre BD de crabe male en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Male_and_Blue$BD,Crabs_Male_and_Orange$BD,col = c("blue","orange"), main = "Males - Comparaison de la variable BD selon l'espèce")

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
boxplot(Crabs_Female_and_Blue$FL,Crabs_Female_and_Orange$FL,col = c("blue","orange"), main = "Femelles - Comparaison de la variable FL selon l'espèce")
#comparer le paramètre RW de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$RW,Crabs_Female_and_Orange$RW,col = c("blue","orange"), main = "Femelles - Comparaison de la variable RW selon l'espèce")
#comparer le paramètre CL de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$CL,Crabs_Female_and_Orange$CL,col = c("blue","orange"), main = "Femelles - Comparaison de la variable CL selon l'espèce")
#comparer le paramètre CW de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$CW,Crabs_Female_and_Orange$CW,col = c("blue","orange"), main = "Femelles - Comparaison de la variable CW selon l'espèce")
#comparer le paramètre BD de crabe femelle  en fonction de l'espèce (bleue ou orange)
boxplot(Crabs_Female_and_Blue$BD,Crabs_Female_and_Orange$BD,col = c("blue","orange"), main = "Femelles - Comparaison de la variable BD selon l'espèce")


#etudier selon le sexe
#comparer le paramètre FL des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$FL,Crabs_Female_and_Blue$FL,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable FL selon le sexe")
#comparer le paramètre RW des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$RW,Crabs_Female_and_Blue$RW,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable RW selon le sexe")
#comparer le paramètre CL des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$CL,Crabs_Female_and_Blue$CL,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable CL selon le sexe")
#comparer le paramètre CW des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$CW,Crabs_Female_and_Blue$CW,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable CW selon le sexe")
#comparer le paramètre BD des crabes d'espèce bleue en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Blue$BD,Crabs_Female_and_Blue$BD,col = c("blue","blue"), main = "Espèce Bleue - Comparaison de la variable BD selon le sexe")

#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$FL,Crabs_Female_and_Orange$FL,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable FL selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$RW,Crabs_Female_and_Orange$RW,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable RW selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$CL,Crabs_Female_and_Orange$CL,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable CL selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$CW,Crabs_Female_and_Orange$CW,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable CW selon le sexe")
#comparer le paramètre FL des crabes d'espèce orange en fonction du sexe (male/femelle)
boxplot(Crabs_Male_and_Orange$BD,Crabs_Female_and_Orange$BD,col = c("orange","orange"), main = "Espèce Orange - Comparaison de la variable BD selon le sexe")


#1.2.2
#corrélation 
#selon le résultat, on voit qu'il existe une forte corrélation entre des variables car la corrélation entre deux différentes variables est presque égale à 1.
cor(crabsquant)

