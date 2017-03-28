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


notes <- read.csv("data/sy02-p2016.csv", na.strings="", header=T)
# summary(notes)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),ordered=T)

################### Question 1 : analyse générale ###################
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



################### Question 2 :analyse statistique  ############
###### Lien Resultat et formation d'origine des étudiants (dernier.diplome.obtenu) ######
t(t(summary(notes$dernier.diplome.obtenu)))
# On a des NA, quels sont-ils ?
notes$statut[which(is.na(notes$dernier.diplome.obtenu))] # résultat = [1] Echange Echange Echange Echange Echange Echange
# => on se rend compte que les 6 données manquantes correspondent aux étudiants venant en échange pour qui ce n'est pas renseigné
# => On va donc les enlever dans "dernier.diplome.obtenu" les lignes possédant des valeurs manquantes (1)
# Et on va aussi enlever dans "resultat" les lignes concernant les étudiants en Echange (2)
formation_origine = notes$dernier.diplome.obtenu[-which(is.na(notes$resultat))] # enlever les absents
formation_origine
formation_origine = na.omit(formation_origine) # (1) # enlever les diplomes non renseignés
#resultats_sans_echange = notes$resultat[-which(notes$statut=="Echange")] # (2)
resultats_sans_echange = notes$resultat[-which(is.na(notes$dernier.diplome.obtenu))] # (2)
resultats_sans_echange
resultats_sans_echange = na.omit(resultats_sans_echange)
# vérifier que les deux nouvelles variables ont la même longueur
length(formation_origine) == length(resultats_sans_echange) # = TRUE => même longueur/dimension
# tableau de contingence
contingence_result_formation = as.data.frame.matrix(table(formation_origine, resultats_sans_echange))
contingence_result_formation
t(t(apply(contingence_result_formation, MARGIN = 1, sum)))
# on remarque que beaucoup de cases du tableau n'ont pas un effectif supérieur ou égal à 5, les conditions pour le test du Chi 2 ne sont donc pas réunies.
# On va quand meme le faire pour voir ce que ça donne
chisq.test(contingence_result_formation)
# Test Chi2 : En général on accepte l'hypothèse d'indépendance lorsque p-value est supérieure à 5 % (0,05).
# on remarque quand même que certains dimplome d'origine semblent influer sur le résultat. Par exemple, beaucoup plus de DUT échouent que les élèves ayant un BAC en dernier dilôme, cad les élèves venant de TC
# donc les conditions du test n'étant pas respectée précédement, nous nous intéressement seulement aux population qui remplissent les conditions, à savoir BAC, CPGE, DUT
contingence_result_formation_BAC_DUT_CPGE = rbind(contingence_result_formation[4,], contingence_result_formation[6,], contingence_result_formation[8,])
contingence_result_formation_BAC_DUT_CPGE
# respecter les conditions du test du chi deux
contingence_result_formation_BAC_DUT_CPGE[,6] = contingence_result_formation_BAC_DUT_CPGE[,6] + contingence_result_formation_BAC_DUT_CPGE[,7]
contingence_result_formation_BAC_DUT_CPGE[,2] = contingence_result_formation_BAC_DUT_CPGE[,1] + contingence_result_formation_BAC_DUT_CPGE[,2]
contingence_result_formation_BAC_DUT_CPGE = contingence_result_formation_BAC_DUT_CPGE[,-7]
contingence_result_formation_BAC_DUT_CPGE = contingence_result_formation_BAC_DUT_CPGE[,-1]
names(contingence_result_formation_BAC_DUT_CPGE) = c("F-Fx", "E", "D", "C", "B-A")
contingence_result_formation_BAC_DUT_CPGE
chisq.test(contingence_result_formation_BAC_DUT_CPGE)
# on obtient un test du Chi2 significatif avec p-value = 0.000288077
# => on peut donc rejeter l'hypothèse d'indépendance => la formation d'origine des étudiants a un impact sur leur résultat final

# Visualisation
options(digits = 6)
contingence_result_formation_BAC_DUT_CPGE
a = sum(contingence_result_formation_BAC_DUT_CPGE[1,])
a
contingence_result_formation_BAC_DUT_CPGE_percentage = contingence_result_formation_BAC_DUT_CPGE
contingence_result_formation_BAC_DUT_CPGE_percentage[1,] = contingence_result_formation_BAC_DUT_CPGE[1,] / sum(contingence_result_formation_BAC_DUT_CPGE[1,]) * 100
contingence_result_formation_BAC_DUT_CPGE_percentage[2,] = contingence_result_formation_BAC_DUT_CPGE[2,] / sum(contingence_result_formation_BAC_DUT_CPGE[2,]) * 100
contingence_result_formation_BAC_DUT_CPGE_percentage[3,] = contingence_result_formation_BAC_DUT_CPGE[3,] / sum(contingence_result_formation_BAC_DUT_CPGE[3,]) * 100
contingence_result_formation_BAC_DUT_CPGE_percentage
dput(head(contingence_result_formation_BAC_DUT_CPGE_percentage))


barplot(as.matrix(contingence_result_formation_BAC_DUT_CPGE_percentage), beside = TRUE,
        col = c("light green", "light blue", "grey"),
        legend = rownames(contingence_result_formation_BAC_DUT_CPGE),
        xlab = "Résultats obtenus", ylab = "Pourcentage d'étudiants par diplôme")



###### Lien Resultat et Spécialité des étudiants (GB, GI, ...) ######
t(t(summary(notes$specialite))) # pas de NA mais on remarque des classes avec peu d'effectif, il va donc falloir les supprimer pour faire un test de Chi2

contingence_resut_speciality = as.data.frame.matrix(table(notes$specialite, notes$resultat))
contingence_resut_speciality
chisq.test(contingence_resut_speciality) #p-value = 0.02088 < 0,05 donc on refute l'hypothèse d'indépendance => les variables sont dépendantes
filtered_contingence_resut_speciality = rbind(
  contingence_resut_speciality[1,], 
  contingence_resut_speciality[2,], 
  contingence_resut_speciality[3,],
  contingence_resut_speciality[5,],
  contingence_resut_speciality[6,]
)
filtered_contingence_resut_speciality
filtered_contingence_resut_speciality[,1] = filtered_contingence_resut_speciality[,1] + filtered_contingence_resut_speciality[,2]
filtered_contingence_resut_speciality[,3] = filtered_contingence_resut_speciality[,3] + filtered_contingence_resut_speciality[,4]
filtered_contingence_resut_speciality[,6] = filtered_contingence_resut_speciality[,6] + filtered_contingence_resut_speciality[,7]
filtered_contingence_resut_speciality = filtered_contingence_resut_speciality[,-c(2,4,7)]
names(filtered_contingence_resut_speciality) = c("F-Fx", "E-D", "C", "B-A")
filtered_contingence_resut_speciality
chisq.test(filtered_contingence_resut_speciality) # p-value = 0.4673856 largement supérieur à 0,05 donc on accepte avec grande confiance l'hypothèse H0
barplot(as.matrix(filtered_contingence_resut_speciality), beside = T)#, legend = rownames(filtered_contingence_resut_speciality))


###### Lien Resultat et leur niveau (GX 1 à 6) #########
t(t(summary(notes$niveau)))
contingence_resut_niveau = as.data.frame.matrix(table(notes$niveau, notes$resultat))
contingence_resut_niveau
chisq.test(contingence_resut_niveau) # condition non remplie, p-value = 0.0003518 => rejet hypothèse indépendance
filtered_contingence_resut_niveau = rbind(
  contingence_resut_niveau[1,],
  contingence_resut_niveau[2,], 
  contingence_resut_niveau[4,]
)
filtered_contingence_resut_niveau
filtered_contingence_resut_niveau[,1] = filtered_contingence_resut_niveau[,1] + filtered_contingence_resut_niveau[,2] + filtered_contingence_resut_niveau[,3]
filtered_contingence_resut_niveau[,5] = filtered_contingence_resut_niveau[,5] + filtered_contingence_resut_niveau[,6]
filtered_contingence_resut_niveau = filtered_contingence_resut_niveau[, -c(2,3,6)]
names(filtered_contingence_resut_niveau) = c("F-Fx-E", "D", "C-B", "A")
filtered_contingence_resut_niveau
chisq.test(filtered_contingence_resut_niveau) # p-value = 0.002587 => rejet hypothèse indépendance, plus le niveau monte (plus le temps passe pour l'étudiant), plus il semble difficile de réussir l'UV

filtered_contingence_resut_niveau_frequence[1,] = filtered_contingence_resut_niveau[1,] / sum(filtered_contingence_resut_niveau[1,]) * 100
filtered_contingence_resut_niveau_frequence[2,] = filtered_contingence_resut_niveau[2,] / sum(filtered_contingence_resut_niveau[2,]) * 100
filtered_contingence_resut_niveau_frequence[3,] = filtered_contingence_resut_niveau[3,] / sum(filtered_contingence_resut_niveau[3,]) * 100
filtered_contingence_resut_niveau_frequence
sum(filtered_contingence_resut_niveau_frequence[1,4:7])
barplot(
  as.matrix(filtered_contingence_resut_niveau_frequence), 
  col = c("light green", "light blue", "grey"),
  beside = T, 
  legend = row.names(filtered_contingence_resut_niveau_frequence),
  xlab = "Résultats obtenus", ylab = "Pourcentage d'étudiants par niveau")


###### Lien Resultat et Correcteur ##########
t(t(summary(notes$correcteur.median)))
t(t(summary(notes$note.median)))
cor_median_clean = na.omit(notes$correcteur.median)
notes_median_clean = na.omit(notes$note.median)
as.data.frame.matrix(table(cor_median_clean, notes_median_clean)) # difficilement utilisable
classes_notes_median_clean = cut(notes_median_clean, 
    breaks = c(-Inf, 8, 11, 14, Inf), 
    labels = c("0-7", "8-10", "11-13", "14-20"), 
    right = FALSE) # regrouper les notes en classes
classes_notes_median_clean
contingence_result_correcteur_median = as.data.frame.matrix(table(cor_median_clean, classes_notes_median_clean))
#contingence_result_correcteur_median = as.data.frame.matrix(table(classes_notes_median_clean, cor_median_clean))
contingence_result_correcteur_median
chisq.test(contingence_result_correcteur_median) # p-value = 0.08142 => on accepte l'hypothèse d'indépendance mais de peu



t(t(summary(notes$correcteur.final)))
t(t(summary(notes$note.final)))
cor_final_clean = na.omit(notes$correcteur.final)
notes_final_clean = na.omit(notes$note.final)
as.data.frame.matrix(table(cor_final_clean, notes_final_clean)) # difficilement utilisable
classes_notes_final_clean = cut(notes_final_clean, 
                                  breaks = c(-Inf, 8, 11, 14, Inf), 
                                  labels = c("0-7", "8-10", "11-13", "14-20"), 
                                  right = FALSE) # regrouper les notes en classes
contingence_result_correcteur_final = contingence_result_correcteur_final[,-2]
contingence_result_correcteur_final = as.data.frame.matrix(table(cor_final_clean, classes_notes_final_clean))
contingence_result_correcteur_final
chisq.test(contingence_result_correcteur_final) # p-value = 0.05737 => on accepte l'hypothèse d'indépendance mais de très peu

