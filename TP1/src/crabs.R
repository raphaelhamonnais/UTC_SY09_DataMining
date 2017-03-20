#1.2 donnees crabs
#installer le package "MASS" qui contient la data "crabs" 
install.packages("MASS")
#charger le package "MASS"
library(MASS)
#morphométrique du crabe: 200 lignes et 8 colonnes, 4 groupes(chaque groupe a 50 crabes), cinq morphométriques (FL: taille de la frontale, RW: largeur de la queue, CL: longueur de la coquille, CW: largeur de la coque, BD: la profondeur du corps), deux type de couleurs (B: bleu, O: orange), le sexe
#crabs:蟹的形态测量: 有200行和8列, 4组每组50只螃蟹，5个形态测量(FL: 额叶尺寸, RW:尾部宽度, CL:壳长, CW:壳宽, BD:身体深度), 两种颜色(B:蓝色, O:橙色), 性别
crabs

#transférer des données au fichier .csv 
#write.csv(crabs, file = "E:/study/UTC/GI05/SY09/TD/TD1/crabs.csv")

#voir la définition et des détails des données “crabs”
help(crabs)

#faire un sommaire 
summary(crabs)

#dessiner les données en utilisant les fonctions plot() et boxplot()
plot(crabs)
boxplot(crabs)

#data(): charger l'ensemble de données spécifiés ou lister l'ensemble de données disponibles
data(crabs)

#cinq morphométriques du crabe: FL, RW, CL, CW, BD
crabsquant <- crabs[,4:8]
crabsquant
summary(crabsquant)
plot(crabsquant)
boxplot(crabsquant)

#le sexe du crabe: male et femelle  
Male <- which(crabs$sex == "M")
Male
Female <- which(crabs$sex == "F")
Female

#l'espèce du crabe: couleur bleu ou couleur orange 
Blue  <- which(crabs$sp == "B")
Blue
Orange <- which(crabs$sp == "O")
Orange

#1.2.1
#desinner selon l'espèce 
plot(crabsquant,col = c("blue","orange")[crabs$sp])
dev.off()
#dessiner selon le sex
plot(crabsquant,col=c("black","red")[crabs$sex])
dev.off()

#étudier selon l'espèce 
#crabes males, avec le numéro 1-50 et 101-150
CM <- crabsquant[Male,]
CM
#crabes males de couleur bleue, avec le numéro 1-50
CMB <- CM[Male == Blue,]
CMB
#crabes males de couleur orange, avec le numéro 101-150
CMO <- CM[!Male == Blue,]
CMO
#comparaison le paramètre FL de crabe male de couleur bleue et orange
boxplot(CMB$FL,CMO$FL,col = c("blue","orange"), main = "Comparaison de la variable FL selon l'espèce")
#comparaison le paramètre RW de crabe male de couleur bleue et orange
boxplot(CMB$RW,CMO$RW,col = c("blue","orange"), main = "Comparaison de la variable RW selon l'espèce")
#comparaison le paramètre CL de crabe male de couleur bleue et orange
boxplot(CMB$CL,CMO$CL,col = c("blue","orange"), main = "Comparaison de la variable CL selon l'espèce")
#comparaison le paramètre CW de crabe male de couleur bleue et orange
boxplot(CMB$CW,CMO$CW,col = c("blue","orange"), main = "Comparaison de la variable CW selon l'espèce")
#comparaison le paramètre BD de crabe male de couleur bleue et orange
boxplot(CMB$BD,CMO$BD,col = c("blue","orange"), main = "Comparaison de la variable BD selon l'espèce")

#crabes femelles, avec le numéro 51-100 et 151-200
CF <- crabsquant[Female,]
CF
#crabes femelles de couleur bleue, avec le numéro 51-100
CFB <- CF[!Female == Orange,]
cFB
#crabes femelles de couleur orange, avec le numéro 151-200
CFO <- CF[Female == Orange,]
CFO
#comparaison le paramètre FL de crabe femelles de couleur bleue et orange
boxplot(CFB$FL,CFO$FL,col = c("blue","orange"), main = "Comparaison de la variable FL selon l'espèce")
#comparaison le paramètre RW de crabe femelles de couleur bleue et orange
boxplot(CFB$RW,CFO$RW,col = c("blue","orange"), main = "Comparaison de la variable RW selon l'espèce")
#comparaison le paramètre CL de crabe femelles de couleur bleue et orange
boxplot(CFB$CL,CFO$CL,col = c("blue","orange"), main = "Comparaison de la variable CL selon l'espèce")
#comparaison le paramètre CW de crabe femelles de couleur bleue et orange
boxplot(CFB$CW,CFO$CW,col = c("blue","orange"), main = "Comparaison de la variable CW selon l'espèce")
#comparaison le paramètre BD de crabe femelles de couleur bleue et orange
boxplot(CFB$BD,CFO$BD,col = c("blue","orange"), main = "Comparaison de la variable BD selon l'espèce")


#etudier en sexe
#crabes de couleur bleue, avec le numéro: 1-100
CB <- crabsquant[Blue,]
CB
#crabes males de couleur bleue, avec le numéro 1-50
CBM <- na.omit(CB[Male,])
CBM
#crabes femelles de couleur bleue, avec le numéro 51-100
CBF <- na.omit(CB[Female,])
CBF
#comparaison le paramètre FL de crabe male et femelle de couleur bleue
boxplot(CBM$FL,CBF$FL,col = c("blue","blue"), main = "Comparaison de la variable FL selon le sex")
#comparaison le paramètre RW de crabe male et femelle de couleur bleue
boxplot(CBM$RW,CBF$RW,col = c("blue","blue"), main = "Comparaison de la variable RW selon le sex")
#comparaison le paramètre CL de crabe male et femelle de couleur bleue
boxplot(CBM$CL,CBF$CL,col = c("blue","blue"), main = "Comparaison de la variable CL selon le sex")
#comparaison le paramètre CW de crabe male et femelle de couleur bleue
boxplot(CBM$CW,CBF$CW,col = c("blue","blue"), main = "Comparaison de la variable CW selon le sex")
#comparaison le paramètre BD de crabe male et femelle de couleur bleue
boxplot(CBM$BD,CBF$BD,col = c("blue","blue"), main = "Comparaison de la variable BD selon le sex")

#crabes de couleur orange, avec lenuméro: 101-200
CO <- crabsquant[Orange,]
CO
#crabes males de couleur bleue, avec le numéro 1-50
COM <- na.omit(CO[Male,])
COM
#crabes femelles de couleur bleue, avec le numéro 51-100
COF <- na.omit(CO[Female,])
COF
#comparaison le paramètre FL de crabe male et femelle de couleur orange
boxplot(COM$FL,COF$FL,col = c("orange","orange"), main = "Comparaison de la variable FL selon le sex")
#comparaison le paramètre RW de crabe male et femelle de couleur orange
boxplot(COM$RW,COF$RW,col = c("orange","orange"), main = "Comparaison de la variable RW selon le sex")
#comparaison le paramètre CL de crabe male et femelle de couleur orange
boxplot(COM$CL,COF$CL,col = c("orange","orange"), main = "Comparaison de la variable CL selon le sex")
#comparaison le paramètre CW de crabe male et femelle de couleur orange
boxplot(COM$CW,COF$CW,col = c("orange","orange"), main = "Comparaison de la variable CW selon le sex")
#comparaison le paramètre BD de crabe male et femelle de couleur orange
boxplot(COM$BD,COF$BD,col = c("orange","orange"), main = "Comparaison de la variable BD selon le sex")


#1.2.2
#corrélation 
#selon le résultat, on voit que il existe une forte corrélation entre des variables car la corrélation entre deux différents variables est presque égale à 1.
cor(crabsquant)

