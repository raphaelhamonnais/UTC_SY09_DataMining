# construire des données pour la régression logistique et calculer les paramètres
log.app <- function(Xapp, zapp, intr, epsi)
{
	n <- dim(Xapp)[1] # nombre d'individu
	p <- dim(Xapp)[2] # nombre de variable

	Xapp <- as.matrix(Xapp)

	if (intr == T) # on ajoute une ordonnée à l'origine
	{
		Xapp <- cbind(rep(1,n),Xapp)
		p <- p + 1
	}

	targ <- matrix(as.numeric(zapp),nrow=n) # ti: la réalisation d'une variable Ti~B(pi)
	targ[which(targ==2),] <- 0              # remplacer la classe 2 à 0
	tXap <- t(Xapp)

	beta <- matrix(0,nrow=p,ncol=1)   

	conv <- F
	iter <- 0
	while (conv == F)
	{
		iter <- iter + 1
		bold <- beta

		prob <- postprob(beta, Xapp) # la valeur de pi
		MatW <- diag(as.numeric(prob * (1 - prob)))    # W: la matrice diagonale de terme Wii = pi(1-pi)

		beta <- bold - (solve(-(tXap%*%MatW%*%Xapp)))%*%tXap%*%(targ-prob) # l'équation de nouvelle esimation 

		if (norm(beta-bold)<epsi)
		{
			conv <- T
		}
	}

	prob <- postprob(beta, Xapp)
	out <- NULL
	out$beta <- beta
	out$iter <- iter
	out$logL <- sum(targ*prob+(1-targ)*(1-prob))

	out
}

# classer un ensemble d'individus
log.val <- function(beta, Xtst)
{
	m <- dim(Xtst)[1]  # nombre d'individu des données de test 
	p <- dim(beta)[1]  
	pX <- dim(Xtst)[2] # nombre de variable des données de test 

	Xtst <- as.matrix(Xtst)

	if (pX == (p-1))
	{
		Xtst  <- cbind(rep(1,m),Xtst)
	}

	prob <- postprob(beta,Xtst) # les probabilités a posteriori
	pred <- max.col(prob) # trouver la position de la valeur maximale pour chaque ligne de matrice

	out <- NULL
	out$prob <- prob
	out$pred <- pred

	return(out)
}


# calculer des probabilités a posteriori
postprob <- function(beta, X)
{
	X <- as.matrix(X)

	prob <- t(exp(t(beta)%*%t(X))/(1+exp(t(beta)%*%t(X)))) # p=t(p1,...,pn)
}

#####################Test########################################
source("src/fonctions/prob.log.R")

appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]

testData <- read.csv("data/Synth1-40.csv")
Xtst <- testData[,1:2]
ztst <- testData[,3]

log1 <- log.app(Xtst, ztst, T, 1e-5)
beta = log1$beta


log2 <- log.app(Xtst, ztst, F, 1e-5)
beta = log2$beta


log.val(beta,Xtst)

prob.log(beta, Xtst, ztst, seq(0,1,0.1))
