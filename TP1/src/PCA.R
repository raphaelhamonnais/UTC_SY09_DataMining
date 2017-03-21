notes <- read.csv("E:/study/UTC/GI05/SY09/TD/TD1/donnees/sy02-p2016.csv", na.strings="", header=T)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
                         ordered=T)
boxplot(notes)
summary(notes)

#calculer les moyennes des notes médian corrigées par chaque correcteur 
#aggregate: Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
#renommer la dataframe “moy.median”
names(moy.median) <- c("correcteur","moy.median")
moy.median
#calculer les variances des notes médian corrigées par chaque correcteur 
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
#calculer les variances des notes finales corrigées par chaque correcteur 
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) <- c("correcteur","std.final")
std.final
final <- merge(moy.final, std.final)
final
# correcteurs <- merge(median, final, all=T)
# correcteurs
# 
# corr.acp <- correcteurs[-c(2,8),]
# corr.acp
#supprimer deux lignes avec des resultats NAs
correcteurs <- merge(median, final)
correcteurs


