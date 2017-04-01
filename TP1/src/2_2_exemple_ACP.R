install.packages("plotrix")
install.packages("xtable")
library("plotrix")
library("xtable")
library("ade4")

#un exemple dans le poly de page 43
math <- t(t(c(6,8,6,14.5,14,11,5.5,13,9)))
math
scie <- t(t(c(6,8,7,14.5,14,10,7,12.5,9.5)))
scie
fran <- t(t(c(5,8,11,15.5,12,5.5,14,8.5,12.5)))
fran
lati <- t(t(c(5.5,8,9.5,15,12.5,7,11.5,9.5,12)))
lati
d_m <- t(t(c(8,9,11,8,10,13,10,12,18)))
d_m
notes <- cbind(math,scie,fran,lati,d_m)
notes
rownames(notes) <- c("jean", "aline", "annie", "monique", "didier", "andre", "pierre", "brigitte", "evelyne")
colnames(notes) <- c("math", "scie", "fran", "lati", "d-m")
notes

#calculer la matrice centrage 
notes_centrage <- scale(notes, center = TRUE, scale = FALSE)
notes_centrage

#calculer la matrice de covariance
notes_covariance <- cov.wt(notes_centrage, method = "ML")
notes_covariance

# diagonaliser la covariance
notes_covariance_diag <- eigen(notes_covariance$cov)
notes_covariance_diag

#valeurs propres
valeurs_propres <- notes_covariance_diag$values
valeurs_propres
#vecteurs propres
vecteurs_propres <- notes_covariance_diag$vectors
vecteurs_propres

# afficher les vecteurs propres en mode "matrice" mathématique
for(iter in 1:dim(vecteurs_propres)[2]) {
  print(iter)
  print(xtable(as.matrix(vecteurs_propres[,iter])), floating=FALSE, tabular.environment="pmatrix", hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE) # print matrix
}

# les pourcentages d'inerties sont simplement la quantité / la somme de la quantité totale d'inertie
pourcentage_inertie_expliquee = valeurs_propres / sum(valeurs_propres) * 100
pourcentage_inertie_expliquee

#la matrice des composantes principales
ACP <- notes_centrage %*% vecteurs_propres
ACP

#Vue que les pourcentages d'inertie expliquée par chaque axes sont 57.69, 24.65, 17.59, 0.04, 0.02
#les pourcentages d'inertie expliquée par les sous-espaces principaux sont 57.69, 82.34, 99.94, 99.98 et 100
#donc le nuage initial est pratiquement dans un espace de dimension 3
#Dessiner le premier plan factoriel entre composant1 et composant2
plot(-9:10,-9:10,type = "n", xlab = "Axe1", ylab = "Axe2")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,2], labels = c("jean", "aline", "annie", "monique", "didier", "andre", "pierre", "brigitte", "evelyne"))

#Dessiner le premier plan factoriel entre composant1 et composant3
plot(-9:10,-9:10,type = "n", xlab = "Axe1", ylab = "Axe3")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,3], labels = c("jean", "aline", "annie", "monique", "didier", "andre", "pierre", "brigitte", "evelyne"))

#Dessiner le premier plan factoriel entre composant1 et composant4
plot(-9:10,-9:10,type = "n", xlab = "Axe1", ylab = "Axe4")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,4], labels = c("jean", "aline", "annie", "monique", "didier", "andre", "pierre", "brigitte", "evelyne"))

#Calculer des contributions 
#contributions relatives des axes aux individus
# apply(notes_centrage^2, 1, sum)
# 1/apply(notes_centrage^2, 1, sum)
# diag(1/apply(notes_centrage^2, 1, sum))
# ACP^2
COR <- diag(1/apply(notes_centrage^2, 1, sum)) %*% ACP^2
COR

#contributions relatives des individus aux axes
n <- dim(notes)[1]
# (1/n)*ACP^2
# 1/diag(valeurs_propres)
# diag(1/diag(valeurs_propres))
# diag(diag(1/diag(valeurs_propres)))
CTR <- (1/n)*ACP^2 %*% diag(diag(1/diag(valeurs_propres)))
CTR

#representation des variables
# sqrt((n - 1)/n)
# apply(notes_centrage,2,sd)
# (sqrt((n - 1)/n)*apply(notes_centrage,2,sd))
# 1/(sqrt((n - 1)/n)*apply(notes_centrage,2,sd))
# diag(1/(sqrt((n - 1)/n)*apply(notes_centrage,2,sd)))
# diag(1/(sqrt((n - 1)/n)*apply(notes_centrage,2,sd))) %*% vecteurs_propres 
# diag(sqrt(valeurs_propres))
cor <- diag(1/(sqrt((n - 1)/n)*apply(notes_centrage,2,sd))) %*% vecteurs_propres %*% diag(sqrt(valeurs_propres))
rownames(cor) <- c("math", "scie", "fran", "lati", "d-m")
colnames(cor) <- c("F1", "F2", "F3", "F4", "F5")
cor

plot(-1:1, -1:1, type = "n", xlab = "Axe1", ylab = "Axe2")
abline(h=0,v=0)
draw.circle(0,0,1)
text(cor[,1], cor[,2], labels = c("math", "scie", "fran", "lati", "d-m"))

plot(-1:1, -1:1, type = "n", xlab = "Axe1", ylab = "Axe3")
abline(h=0,v=0)
draw.circle(0,0,1)
text(cor[,1], cor[,3], labels = c("math", "scie", "fran", "lati", "d-m"))

plot(-1:1, -1:1, type = "n", xlab = "Axe1", ylab = "Axe4")
abline(h=0,v=0)
draw.circle(0,0,1)
text(cor[,1], cor[,4], labels = c("math", "scie", "fran", "lati", "d-m"))

#Utiliser la fonction princomp() pour calculer les composantes principales 
#On utilise la matrice de covariance pour calculer les composantes principales 
PCA <- princomp(notes, cor = FALSE)
PCA
summary(PCA, loadings = TRUE)
#les écarts-types des composantes principales 
PCA$sdev
#les vecteurs propres 
PCA$loadings
#les données des composantes principales
PCA$scores

#utiliser la fonction plot
#Dessiner le premier plan factoriel entre composant1 et composant2
plot(-9:10,-9:10,type = "n", xlab = "Axe1", ylab = "Axe2")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(PCA$scores[,1], PCA$scores[,2], labels = c("jean", "aline", "annie", "monique", "didier", "andre", "pierre", "brigitte", "evelyne"))

#Dessiner le premier plan factoriel entre composant1 et composant3
plot(-9:10,-9:10,type = "n", xlab = "Axe1", ylab = "Axe3")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(PCA$scores[,1], PCA$scores[,3], labels = c("jean", "aline", "annie", "monique", "didier", "andre", "pierre", "brigitte", "evelyne"))

#notes
#utiliser la fonction biplot
cor(notes, PCA$scores)
biplot(PCA, choices = c(1,2))
biplot(PCA, choices = c(1,3))
