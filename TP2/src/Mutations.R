mut <- read.csv("data/mutations2.csv", header=T, row.names = 1)
mut
plot(mut)
summary(mut)
dim(mut)
#faire la tableau de dissimilarite
mut <- as.dist(mut, diag = T, upper = T)
mut
#calculer la presentation euclidienne des donnees par AFTD
cmdscale_mut <- cmdscale(mut, k = 2, eig = TRUE)
cmdscale_mut
