#1.3 Données Pima
setwd("E:/study/UTC/GI05/SY09/UTC_SY09_TPs/TP1")
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
Pima$z
class(Pima$z)
