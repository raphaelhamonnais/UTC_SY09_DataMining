install.packages("plotrix")
library("plotrix")

notes <- read.csv("data/sy02-p2016.csv", na.strings="", header=T)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
                         ordered=T)
notes$correcteur.median <- factor(notes$correcteur.median, levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$correcteur.final <- factor(notes$correcteur.final, levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
boxplot(notes)
summary(notes)



###########################################
# 2.1 Exercice théorique

#calculer les moyennes des notes médian corrigées par chaque correcteur 
#aggregate: Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
#renommer la dataframe moy.median
moy.median
names(moy.median) <- c("correcteur","moy.median")
moy.median
#calculer les écart-types des notes médian corrigées par chaque correcteur 
std.median <- aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) <- c("correcteur","std.median")
std.median
#mettre deux datadframe ensemble 
median <- merge(moy.median, std.median)
median
#calculer les moyennes des notes finales corrigées par chaque correcteur 
moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) <- c("correcteur","moy.final")
moy.final
#calculer les écarts types des notes finales corrigées par chaque correcteur 
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) <- c("correcteur","std.final")
std.final
final <- merge(moy.final, std.final)
final
correcteurs <- merge(median, final, all=T)

# supprimer deux lignes avec des resultats NAs
correcteurs
corr.acp <- correcteurs[-c(2,3),] # on peut aussi faire merge(median, final) sans le "all=T"
corr.acp


# 1. Calculer les axes factoriels de l'ACP du nuage de points défini
# par les quatre variables quantitatives.
# Quels sont les pourcentages d'inertie expliquée par chacun de ces axes ?
corr.acp <- corr.acp[,-c(1)] # ne prendre que les variables quantitatives
corr.acp
corr.acp.centered = scale(corr.acp, center = TRUE, scale = FALSE) # centrer les données en colonne
corr.acp.centered
corr.acp.covar = cov.wt(corr.acp.centered, method = "ML") # calcul de matrice de covariance avec centrage et réduction automatique
corr.acp.covar
corr.acp.covar.diago = eigen(corr.acp.covar$cov) # diagonaliser la covariance (pas les moyennes)
val_propres = corr.acp.covar.diago$values # valeurs propres = quantités d'inertie expliquée
vec_propres = corr.acp.covar.diago$vectors # vecteurs propres = vecteurs qui portent/expliquent les axes factoriels
# normalement matrice de covariance = corr.acp.covar.diago$vectors * corr.acp.covar.diago$values * t(corr.acp.covar.diago$vectors)
vec_propres %*% diag(val_propres) %*% t(vec_propres)
corr.acp.covar$cov
# on remarque que les deux sont egales, même si le comparateur "==" ne marche pas car pas problème de précision

# Calculer les axes factoriels de l'ACP
# les axes factoriels sont donnés par les vecteurs propres situés dans vec_propres : chaque colonne est un des vecteur propre
vec_propres 
# les quantités d'inertie expliquées sont dans la variable val_propres
val_propres
# les pourcentages d'inerties sont simplement la quantité / la somme de la quantité totale d'inertie
pourcentage_inertie = val_propres / sum(val_propres) * 100
pourcentage_inertie

# 2. Calculer les composantes principales ; en déduire la représentation des six individus dans le premier plan factoriel.
ACP = corr.acp.centered %*% vec_propres
ACP # coordonnées des points sur le nouveau repère des composantes principales = combinaisons linéaires des anciens axes

#Nommer chaque ligne des quatre composants principales
rownames(ACP) <- c("Cor1", "Cor4", "Cor5", "Cor6", "Cor7", "Cor8")
ACP
#Vue que les pourcentages d'inertie expliquée par chaque axes sont 66.095613, 24.786460, 5.610544, 3.507382,
#les pourcentages d'inertie expliquée par les sous-espaces principaux sont 66.095613, 90.882073, 96.492618, 100
#donc le nuage initial est pratiquement dans un espace de dimension 3
#Dessiner le premier plan factoriel entre composant1 et composant2
plot(-2:2,-2:2,type = "n", xlab = "Axe1", ylab = "Axe2")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,2], labels = c("Cor1", "Cor4", "Cor5", "Cor6", "Cor7", "Cor8"))

#Dessiner le premier plan factoriel entre composant1 et composant3
plot(-2:2,-2:2,type = "n", xlab = "Axe1", ylab = "Axe3")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,3], labels = c("Cor1", "Cor4", "Cor5", "Cor6", "Cor7", "Cor8"))

#Dessiner le premier plan factoriel entre composant1 et composant4
plot(-2:2,-2:2,type = "n", xlab = "Axe1", ylab = "Axe4")
abline(h=0,v=0)
#Ajouter des points dans le premier plan factoriel 
text(ACP[,1], ACP[,3], labels = c("Cor1", "Cor4", "Cor5", "Cor6", "Cor7", "Cor8"))



#3.Tracer la présentation des quatre variables dans le premier plan factoriel 
plot(-1:1, -1:1, type = "n", xlab = "Axe1", ylab = "Axe2")
abline(h=0,v=0)
draw.circle(0,0,1)
text(cor_acp[,1], cor_acp[,2])




# Question 2.1.3
# formule page 41 tout en bas
# Xj transposé = j ème colonne, = coordonées de individus sur le j ème axe
# C alpha = coordonnées des individus suivant le apha-ième axe de l'ACP
# les Xj sont centrés en colonnes et du coup les C alpha aussi => moyenne égale à 0

# Calcul des covariances entre les variables initiales (x) et le composantes principales trouvées (c)
x = corr.acp
lambda = val_propres
c = ACP
x
lambda
c
var1_comp1 = 1/sd(x[,1]) * 1/sqrt(lambda[1]) * (t(x[,1]) %*% diag(1/6, nrow = 6, ncol = 6) %*% c[,1])
var2_comp1 = 1/sd(x[,2]) * 1/sqrt(lambda[1]) * (t(x[,2]) %*% diag(1/6, nrow = 6, ncol = 6) %*% c[,1])
var1_comp2 = 1/sd(x[,1]) * 1/sqrt(lambda[2]) * (t(x[,1]) %*% diag(1/6, nrow = 6, ncol = 6) %*% c[,2])
var2_comp2 = 1/sd(x[,2]) * 1/sqrt(lambda[2]) * (t(x[,2]) %*% diag(1/6, nrow = 6, ncol = 6) %*% c[,2])
covar_entre_var_et_compo = c(var1_comp1, var1_comp2, var2_comp1, var2_comp2)
covar_entre_var_et_compo
#curve(sqrt(1-x^2),-1,1,add = TRUE)
#curve(-sqrt(1-x^2),-1,1,add = TRUE)


a = princomp(corr.acp)
a$scores == ACP
biplot(princomp(corr.acp))