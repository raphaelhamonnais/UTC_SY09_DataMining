#1.3 Données Pima
Pima <- read.csv("data/Pima.csv",header = T)
summary(Pima)
plot(Pima)
Pima$z <- factor(Pima$z)
dim(Pima)
#supprimer la dernière colonne 
Pima <- Pima[,-ncol(Pima)]
Pima
#prendre les six premirèes lignes
head(Pima)
summary(Pima)

hist(Pima$npreg)
plot(density(Pima$glu))
#sort(): pour faire l 'ordre des données
plot(sort(Pima$bp))
plot(Pima$ped~Pima$bp)
boxplot(Pima$ped~Pima$glu)

cor(Pima)
