#construire des données pour la régression logistique quadratique
log_quad <- function(X)
{
  # données d'origine V1 et V2
  X2 <- X
  
  # calculer des données supplémentaires V1*V2 
  for (p in 1:(dim(X)[2]-1))
  {
    for (q in (p+1):dim(X)[2])
    {
      X2 <- cbind(X2, X[,p]*X[,q])
    }
  }
  
  # calculer des données supplémentaires V1^2 et V2^2
  for (p in 1:dim(X)[2])
  {
    X2 <- cbind(X2, X[,p]^2)
  }
  
  # nommer des nouvelles données par colonne 
  colnames(X2) <- c('V1','V2','V1*V2','V1^2','V2^2')
  
  X <- X2
}

# calculer des paramètres de modèle
log_quad.app <- function(Xapp, zapp, intr, epsi)
{
  Xapp <- log_quad(Xapp)
  
	n <- dim(Xapp)[1] # nombre d'individu
	p <- dim(Xapp)[2] # nombre de variable

	Xapp <- as.matrix(Xapp)

	if (intr == T) # on ajoute une ordonnée à l'origine
	{
		Xapp <- cbind(rep(1,n),Xapp)
		p <- p + 1
	}

	targ <- matrix(as.numeric(zapp),nrow=n) # ti: la réalisation d'une variable Ti~B(pi) 
	targ[which(targ==2),] <- 0 # remplacer la classe 2 à 0
	tXap <- t(Xapp)

	beta <- matrix(0,nrow=p,ncol=1) # w: le paramètre pour calculer 
	rownames(beta) <- colnames(Xapp)
	
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
	out$X <- Xapp
	out$z <- targ
	
	out
}

log_quad.val <- function(beta, Xtst)
{
  #Xtst <- log_quad(Xtst)
  
	m <- dim(Xtst)[1] # nombre d'individu des données de test 
	p <- dim(beta)[1]
	pX <- dim(Xtst)[2] # nombre de variable des données de test 

	Xtst <- as.matrix(Xtst)

	if (pX == (p-1))
	{
		Xtst  <- cbind(rep(1,m),Xtst)
	}

	prob_w1 <- postprob(beta, Xtst) # proportions à postériori de la classe 1
	prob_w2 <- 1 - prob_w1
	prob <- cbind(prob_w1, prob_w2) # les probabilités a posteriori
	pred <- max.col(prob) # trouver la position de la valeur maximale pour chaque ligne de matrice

	out <- NULL
	out$prob <- prob
	out$pred <- pred

	return(out)
}

#calculer des probabilités a posteriori
postprob <- function(beta, X)
{
	X <- as.matrix(X)

	prob <- t(exp(t(beta)%*%t(X))/(1+exp(t(beta)%*%t(X))))
}


#####################Test########################################
source("src/fonctions/prob.log2.R")

appData <- read.csv("data/Synth1-40.csv")
Xapp <- appData[,1:2]
zapp <- appData[,3]


testData <- read.csv("data/Synth1-40.csv")
Xtst <- testData[,1:2]
ztst <- testData[,3]

# situation avec le coordonnée d'origine 
log_quad1 <- log_quad.app(Xtst, ztst, T, 1e-5)
Xtst1 <- log_quad1$X
beta1 = log_quad1$beta
log_quad.val(beta1,Xtst1)

prob.log2(beta1, Xtst, ztst, seq(0,1,0.2))

# situation sans le coordonnée d'origine 
log_quad2 <- log_quad.app(Xtst, ztst, F, 1e-5)
Xtst2 <- log_quad2$X
beta2 = log_quad2$beta

log_quad.val(beta2,Xtst2)

prob.log2(beta2, Xtst, ztst, seq(0,1,0.25)) 

