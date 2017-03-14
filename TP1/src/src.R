# boxplot(x~y) diagramme en barre de x en fonction de y

# factor = variable qualitative
#   => qual ordinale (ordonée) ou nominale (simple nom)
# class(x) où X variable => donne la classe (entier, numerique, double, factor pour variable qualitative)
#   => donne les "levels" de la variable, les valeurs qu'elle peut prendre


# Tri
# which(label == valeur) = selectionner données avec label = valeur
# -which(label == valeur) = selectionner données avec label != valeur


# Remove Not Available NA
# na.omit()
# na.rm=T argument de fonction, mais semble ne pas marcher



# Tableau de contingence pour deux variables qualitatives
tab_contingeance = table(notes$specialite, notes$resultat) # tableau de contingeance pour chaque spécialité
barplot(tab_contingeance, beside = T) # diagramme en baton des resultats par branche

# ------------------------------
# 1.1 Notes
# ------------------------------
notes <- read.csv("data/sy02-p2016.csv", na.strings="", header=T)
# summary(notes)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),ordered=T)

# Question 1 : analyse générale 
dim(notes) # volume

# outil pour la corrélation de deux variables quantitatives
#   => test du Chi2 (X2) d'indépendance (hypothèse = indépendance)
#    => vérifier condition n*prob théorique sous hyp h0 supérieure ou égale à 5

boxplot(notes$niveau~notes$resultat) # ne semble pas très lié, et beaucoup de GX02 à cause de la médiane sur la valeur 2
boxplot(notes$note.totale~notes$specialite) # légèrement lié, mais attention au nombre d'étudiants dans chaque catégorie (ex : HuTech = un seul étudiant)
boxplot(notes$note.totale~notes$resultat) # lié, logique...
plot(notes$note.median,notes$note.final) # peu lié
boxplot(notes$note.median~notes$correcteur.median) # un peu lié mais à étudier
boxplot(notes$note.final~notes$correcteur.final) # un peu lié mais à étudier
boxplot(notes$note.totale~notes$dernier.diplome.obtenu) # lié




# Lien Resultat et formation d'origine des étudiants (dernier.diplome.obtenu)
table(notes$dernier.diplome.obtenu, notes$resultat) # tableau de contingeance dernier.diplome.obtenu et resultat
apply(table(notes$dernier.diplome.obtenu, notes$resultat), 1,sum) # compter le nombre de resultats par dernier diplome obtenu
# faire test Chi2 sur tableau de contingeance

# Lien Resultat et Spécialité des étudiants (dernier.diplome.obtenu)
table(notes$specialite, notes$resultat)# tableau de contingeance dernier.diplome.obtenu et resultat
