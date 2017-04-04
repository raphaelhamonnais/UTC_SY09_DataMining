data(iris)
iris
summary(iris)
iris_quant <- iris[,-5]
iris_quant
dim(iris_quant)
#1.1 affiche le premier plan factoriel 
#faire l'acp 
acp_iris <- princomp(iris_quant)
acp_iris
#affiche le premier plan factoriel sans tenir compte de l'espece
plot(acp_iris$scores)
##affiche le premier plan factoriel en tenant compte de l'espece
plot(acp_iris$scores, col = c("blue", "orange", "green")[iris$Species], asp = 1)

