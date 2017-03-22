#1.3 Donnees Pima
Pima <- read.csv("E:/study/UTC/GI05/SY09/TD/TD1/donnees/Pima.csv",header = T)
summary(Pima)
plot(Pima)
Pima$z <- factor(Pima$z)
dim(Pima)
#supprimer la derni¨¨re colonne 
Pima <- Pima[,-ncol(Pima)]
Pima
#prendre les premiers six lignes 
head(Pima)
summary(Pima)

hist(Pima$npreg)
plot(density(Pima$glu))
#sort(): pour faire l 'ordre des donnees
plot(sort(Pima$bp))
plot(Pima$ped~Pima$bp)
boxplot(Pima$ped~Pima$glu)

cor(Pima)
