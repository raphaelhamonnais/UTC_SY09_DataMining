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
# 2.1 Exercice th茅orique

#calculer les moyennes des notes m茅dian corrig茅es par chaque correcteur 
#aggregate: Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
#renommer la dataframe 鈥渕oy.median鈥?
moy.median
names(moy.median) <- c("correcteur","moy.median")
moy.median
#calculer les 茅cart-types des notes m茅dian corrig茅es par chaque correcteur 
std.median <- aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) <- c("correcteur","std.median")
std.median
#mettre deux datadframe ensemble 
median <- merge(moy.median, std.median)
median
#calculer les moyennes des notes finales corrig茅es par chaque correcteur 
moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) <- c("correcteur","moy.final")
moy.final
#calculer les 茅carts types des notes finales corrig茅es par chaque correcteur 
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


# 1. Calculer les axes factoriels de l鈥橝CP du nuage de points d茅fini
# par les quatre variables quantitatives.
# Quels sont les pourcentages d鈥檌nertie expliqu茅e par chacun de ces axes ?
corr.acp <- corr.acp[,-c(1)] # ne prendre que les variables quantitatives
corr.acp
corr.acp.centered = scale(corr.acp, center = TRUE, scale = FALSE) # centrer les donn茅es en colonne
corr.acp.centered
corr.acp.covar = cov.wt(corr.acp.centered, method = "ML") # calcul de matrice de covariance avec centrage et r茅duction automatique
corr.acp.covar
corr.acp.covar.diago = eigen(corr.acp.covar$cov) # diagonaliser la covariance (pas les moyennes)
val_propres = corr.acp.covar.diago$values # valeurs propres = quantit茅s d'inertie expliqu茅e
vec_propres = corr.acp.covar.diago$vectors # vecteurs propres = vecteurs qui portent/expliquent les axes factoriels
# normalement matrice de covariance = corr.acp.covar.diago$vectors * corr.acp.covar.diago$values * t(corr.acp.covar.diago$vectors)
vec_propres %*% diag(val_propres) %*% t(vec_propres)
corr.acp.covar$cov
# on remarque que les deux sont egales, m锚me si le comparateur "==" ne marche pas car pas probl猫me de pr茅cision

# Calculer les axes factoriels de l鈥橝CP
# les axes factoriels sont donn茅s par les vecteurs propres situ茅s dans vec_propres : chaque colonne est un des vecteur propre
vec_propres 
# les quantit茅s d'inertie expliqu茅es sont dans la variable val_propres
val_propres
# les pourcentages d'inerties sont simplement la quantit茅 / la somme de la quantit茅 totale d'inertie
pourcentage_inertie = val_propres / sum(val_propres) * 100
pourcentage_inertie

# 2. Calculer les composantes principales ; en d茅duire la repr茅sentation des quatre individus dans le premier plan factoriel.
ACP = corr.acp.centered %*% vec_propres
ACP # coordonn茅es des points sur le nouveau rep猫re des composantes principales = combinaisons lin茅aires des anciens axes

