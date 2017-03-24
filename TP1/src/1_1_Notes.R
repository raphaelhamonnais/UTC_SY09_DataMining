# boxplot(x~y) diagramme en barre de x en fonction de y

# factor = variable qualitative
#   => qual ordinale (ordon¨¦e) ou nominale (simple nom)
# class(x) oÃ¹ X variable => donne la classe (entier, numerique, double, factor pour variable qualitative)
#   => donne les "levels" de la variable, les valeurs qu'elle peut prendre


# Tri
# which(label == valeur) = selectionner donn¨¦es avec label = valeur
# -which(label == valeur) = selectionner donn¨¦es avec label != valeur


# Remove Not Available NA
# na.omit()
# na.rm=T argument de fonction, mais semble ne pas marcher



# Tableau de contingence pour deux variables qualitatives
tab_contingeance = table(notes$specialite, notes$resultat) # tableau de contingeance pour chaque sp¨¦cialit¨¦
barplot(tab_contingeance, beside = T) # diagramme en baton des resultats par branche

# ------------------------------
# 1.1 Notes
# ------------------------------
notes <- read.csv("data/sy02-p2016.csv", na.strings="", header=T)
# summary(notes)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$correcteur.median <- factor(notes$correcteur.median,
                                  levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$correcteur.final <- factor(notes$correcteur.final,
                                   levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),ordered=T)

# Question 1 : analyse g¨¦n¨¦rale 
dim(notes) # volume

# outil pour la corr¨¦lation de deux variables quantitatives
#   => test du Chi2 (X2) d'ind¨¦pendance (hypoth¨¨se = ind¨¦pendance)
#    => v¨¦rifier condition n*prob th¨¦orique sous hyp h0 sup¨¦rieure ou ¨¦gale ¨¤ 5

boxplot(notes$niveau~notes$resultat) # ne semble pas tr¨¨s li¨¦, et beaucoup de GX02 ¨¤ cause de la m¨¦diane sur la valeur 2
boxplot(notes$note.totale~notes$specialite) # l¨¦g¨¨rement li¨¦, mais attention au nombre d'¨¦tudiants dans chaque cat¨¦gorie (ex : HuTech = un seul ¨¦tudiant)
boxplot(notes$note.totale~notes$resultat) # li¨¦, logique...
plot(notes$note.median,notes$note.final) # peu li¨¦
boxplot(notes$note.median~notes$correcteur.median) # un peu li¨¦ mais ¨¤ ¨¦tudier
boxplot(notes$note.final~notes$correcteur.final) # un peu li¨¦ mais ¨¤ ¨¦tudier
boxplot(notes$note.totale~notes$dernier.diplome.obtenu) # li¨¦



# Question 2

### Lien Resultat et formation d'origine des ¨¦tudiants (dernier.diplome.obtenu)
t(t(summary(notes$dernier.diplome.obtenu)))
# On a des NA, quels sont-ils ?
notes$statut[which(is.na(notes$dernier.diplome.obtenu))] # r¨¦sultat = [1] Echange Echange Echange Echange Echange Echange
# => on se rend compte que les 6 donn¨¦es manquantes correspondent aux ¨¦tudiants venant en ¨¦change pour qui ce n'est pas renseign¨¦
# => On va donc les enlever dans "dernier.diplome.obtenu" les lignes poss¨¦dant des valeurs manquantes (1)
# Et on va aussi enlever dans "resultat" les lignes concernant les ¨¦tudiants en Echange (2)
formation_origine = notes$dernier.diplome.obtenu[-which(is.na(notes$resultat))] # enlever les absents
formation_origine
formation_origine = na.omit(formation_origine) # (1) # enlever les diplomes non renseign¨¦s
#resultats_sans_echange = notes$resultat[-which(notes$statut=="Echange")] # (2)
resultats_sans_echange = notes$resultat[-which(is.na(notes$dernier.diplome.obtenu))] # (2)
resultats_sans_echange
resultats_sans_echange = na.omit(resultats_sans_echange)
# v¨¦rifier que les deux nouvelles variables ont la m¨ºme longueur
length(formation_origine) == length(resultats_sans_echange) # = TRUE => m¨ºme longueur/dimension
# tableau de contingence
contingence_result_formation = as.data.frame.matrix(table(formation_origine, resultats_sans_echange))
contingence_result_formation
# on remarque que beaucoup de cases du tableau n'ont pas un effectif sup¨¦rieur ou ¨¦gal ¨¤ 5, les conditions pour le test du Chi 2 ne sont donc pas r¨¦unies.
# On va quand meme le faire pour voir ce que Ã§a donne
chisq.test(contingence_result_formation)
# Test Chi2 : En g¨¦n¨¦ral on accepte l'hypoth¨¨se d'ind¨¦pendance lorsque p-value est sup¨¦rieure ¨¤ 5 % (0,05).
# on remarque quand m¨ºme que certains dimplome d'origine semblent influer sur le r¨¦sultat. Par exemple, beaucoup plus de DUT ¨¦chouent que les ¨¦l¨¨ves ayant un BAC en dernier dilÃ´me, cad les ¨¦l¨¨ves venant de TC
# donc les conditions du test n'¨¦tant pas respect¨¦e pr¨¦c¨¦dement, nous nous int¨¦ressement seulement aux population qui remplissent les conditions, ¨¤ savoir BAC, CPGE, DUT
contingence_result_formation_BAC_DUT_CPGE = rbind(contingence_result_formation[4,], contingence_result_formation[6,], contingence_result_formation[8,])
contingence_result_formation_BAC_DUT_CPGE
chisq.test(contingence_result_formation_BAC_DUT_CPGE)
# Bien que certains effectifs sont en dessous de 5, on obtient quand m¨ºme un test du Chi2 significatif avec p-value = 0.0006389
# => on peut donc rejeter l'hypoth¨¨se d'ind¨¦pendance => la formation d'origine des ¨¦tudiants a un impact sur leur r¨¦sultat final





### Lien Resultat et Sp¨¦cialit¨¦ des ¨¦tudiants (GB, GI, ...)
t(t(summary(notes$specialite))) # pas de NA mais on remarque des classes avec peu d'effectif, il va donc falloir les supprimer pour faire un test de Chi2

contingence_resut_speciality = as.data.frame.matrix(table(notes$specialite, notes$resultat))
contingence_resut_speciality
chisq.test(contingence_resut_speciality) #p-value = 0.02088 < 0,05 donc on refute l'hypoth¨¨se d'ind¨¦pendance => les variables sont d¨¦pendantes
filtered_contingence_resut_speciality = rbind(
  contingence_resut_speciality[1,], 
  contingence_resut_speciality[2,], 
  contingence_resut_speciality[3,],
  contingence_resut_speciality[5,],
  contingence_resut_speciality[6,]
)
filtered_contingence_resut_speciality
chisq.test(filtered_contingence_resut_speciality) # p-value = 0.4853 largement sup¨¦rieur ¨¤ 0,05 donc on accepte avec grande confiance l'yphoth¨¨se d'un
plot(filtered_contingence_resut_speciality) # ?????????? comment mod¨¦liser graphiquement ?


### Lien Resultat et leur niveau (GX 1 ¨¤ 6)
t(t(summary(notes$niveau))) 
contingence_resut_niveau = as.data.frame.matrix(table(notes$niveau, notes$resultat))
contingence_resut_niveau
chisq.test(contingence_resut_niveau) # condition non remplie, p-value = 0.0003518 => rejet hypoth¨¨se ind¨¦pendance
filtered_contingence_resut_niveau = rbind(
  contingence_resut_niveau[1,], 
  contingence_resut_niveau[2,], 
  contingence_resut_niveau[4,]
)
filtered_contingence_resut_niveau
chisq.test(filtered_contingence_resut_niveau) # p-value = 0.002587 => rejet hypoth¨¨se ind¨¦pendance, plus le niveau monte (plus le temps passe pour l'¨¦tudiant), plus il semble difficile de r¨¦ussir l'UV



### Lien Resultat et Correcteur
t(t(summary(notes$correcteur.median)))
t(t(summary(notes$note.median)))
cor_median_clean = na.omit(notes$correcteur.median)
notes_median_clean = na.omit(notes$note.median)
as.data.frame.matrix(table(cor_median_clean, notes_median_clean)) # difficilement utilisable
classes_notes_median_clean = cut(notes_median_clean, 
    breaks = c(-Inf, 6, 10, 14, 16, Inf), 
    labels = c("0-5", "6-9", "10-13", "14-15", "16-20"), 
    right = FALSE) # regrouper les notes en classes
contingence_result_correcteur_median = as.data.frame.matrix(table(cor_median_clean, classes_notes_median_clean))
#contingence_result_correcteur_median = as.data.frame.matrix(table(classes_notes_median_clean, cor_median_clean))
contingence_result_correcteur_median
chisq.test(contingence_result_correcteur_median) # p-value = 0.08142 => on accepte l'hypoth¨¨se d'ind¨¦pendance mais de peu



t(t(summary(notes$correcteur.final)))
t(t(summary(notes$note.final)))
cor_final_clean = na.omit(notes$correcteur.final)
notes_final_clean = na.omit(notes$note.final)
as.data.frame.matrix(table(cor_final_clean, notes_final_clean)) # difficilement utilisable
classes_notes_final_clean = cut(notes_final_clean, 
                                 breaks = c(-Inf, 6, 10, 14, 16, Inf), 
                                 labels = c("0-5", "6-9", "10-13", "14-15", "16-20"), 
                                 right = FALSE) # regrouper les notes en classes
contingence_result_correcteur_final = as.data.frame.matrix(table(cor_final_clean, classes_notes_final_clean))
contingence_result_correcteur_final
chisq.test(contingence_result_correcteur_final) # p-value = 0.05737 => on accepte l'hypoth¨¨se d'ind¨¦pendance mais de tr¨¨s peu
