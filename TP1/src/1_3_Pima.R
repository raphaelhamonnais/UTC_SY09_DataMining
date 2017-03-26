#1.3 Données Pima
Pima <- read.csv("data/Pima.csv",header = T)
summary(Pima)
plot(Pima)
Pima$z <- factor(Pima$z)
dim(Pima)
#supprimer la dernière colonne 
Pima <- Pima[,-ncol(Pima)]
Pima
#prendre les six premières lignes
head(Pima)
summary(Pima)

hist(Pima$npreg)
plot(density(Pima$glu))
#sort(): pour faire l 'ordre des données
plot(sort(Pima$bp))
plot(Pima$ped~Pima$bp)
boxplot(Pima$npreg~Pima$z)
boxplot(Pima$glu~Pima$z)
boxplot(Pima$bp~Pima$z)
boxplot(Pima$skin~Pima$z)
boxplot(Pima$bmi~Pima$z)
boxplot(Pima$ped~Pima$z)
boxplot(Pima$age~Pima$z)
cor(Pima)


a = table(Pima$z[which(Pima$z == 1)], Pima$npreg[which(Pima$z == 1)])
a = as.data.frame(table(Pima$z, Pima$npreg))
a
sum(a[1,])
sum(a[2,])
table(Pima$z, Pima$glu)
plot(Pima$z~Pima$glu)
table(Pima$z, Pima$bp)
table(Pima$z, Pima$skin)
table(Pima$z, Pima$bmi)
table(Pima$z, Pima$ped)
table(Pima$z, Pima$age)



acp = princomp(Pima)
acp
biplot(acp)

acp$scores
plot(acp$scores[,2]~acp$scores[,1], col = c("blue","orange")[Pima$z])
plot(acp$scores[,1]~acp$scores[,7], col = c("blue","orange")[Pima$z])




Pima$z
class(Pima$z)

