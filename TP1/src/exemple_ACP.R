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
vecteurs_propres <- (notes_covariance_diag$vectors)
vecteurs_propres

#la matrice des composantes principales
ACP <- notes_centrage%*%vecteurs_propres
ACP

#Vue que les pourcentages d'inertie expliquée par chaque axes sont 57.69, 24.65, 17.59, 0.04, 0.02
#les pourcentages d'inertie expliquée par les sous-espaces principaux sont 57.69, 82.34, 99.94, 99.98 et 100
#donc le nuage initial est pratiquement dans un espace de dimension 3
#Dessiner le premier plan factoriel entre composant1 et composant2
plot(-9:10,-9:10,type = "n", xlab = "Axe1", ylab = "Axe2")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,2])

#Dessiner le premier plan factoriel entre composant1 et composant3
plot(-9:10,-9:10,type = "n", xlab = "Axe1", ylab = "Axe3")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,3])

#Dessiner le premier plan factoriel entre composant1 et composant4
plot(-9:10,-9:10,type = "n", xlab = "Axe1", ylab = "Axe4")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,4])



