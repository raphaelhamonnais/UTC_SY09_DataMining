setwd("~/Documents/Etudes_Informatique/Cours/UTC/GI04/SY09_Data_Mining/TPs")
x<-c(2,4,3,7,1)
A<-matrix(c(1,2,5,3,0,9),nrow=3,byrow=T)
max(x)
max(A)
apply(A,MARGIN = 1,max) # applique max a toutes les lignes (MARGIN=1)
apply(A,MARGIN = 2,max) # applique max a toutes les colonnes (MARGIN=2)
mean(x)
mean(A)
apply(A,MARGIN = 2,mean) # applique mean a toutes les colonnes (MARGIN=2)


# Que calculent les fonctions var et sd ?
var(x)
var(A)
sd(x)
sd(A)

# On comparera les résultats des fonctions var et cov.wt
var(A)
cov.wt(A, method="unbiased") # calcul de la covariance avec un estimateur de la variance non biaisé
cov.wt(A, method = "ML") # calcul de la covariance avec l'estimateur du maximum de vraisemblance

# Les fonctions hist et plot permettent de faire des affichages simples :
x<-c(2,2,2,1,3,4,1,1)
hist(x)
x<-c(1,2,3,4,5)
y<-c(1,4,9,16,25)
plot(x,y)
plot(x,y,pch=22)
plot(x,y,pch=19,col="blue")
plot(x,y,type="l",col="blue")


# Exercice
# Variance = 1/n * transposer(Xc) * Xc où Xc est la colonne c de la matrice X
# Variance empirique = 1/(n-1) * transposer(Xc) * Xc où Xc est la colonne c de la matrice X


source("TP0/center.R")
n = dim(A)[1]
B = apply(A, MARGIN = 2, center)
C = prodtrans(B)
D = (1/n)*B
D


#

