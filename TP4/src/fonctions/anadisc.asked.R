adq.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		param$MCov[,,k] <- var(Xapp[indk,])
		param$mean[k,] <- apply(Xapp[indk,], MARGIN = 2, mean)
		param$prop[k] <- table(zapp)[k] / length(zapp)
	}

	param
}

adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		param$mean[k,] <- apply(Xapp[indk,], MARGIN = 2, mean)
		param$prop[k] <- table(zapp)[k] / length(zapp)
		MCov <- MCov + ( param$prop[k] * var(Xapp[indk,]) )
	}
	MCov <- 1/n * MCov
	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov
	}

	param
}

nba.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		param$MCov[,,k] <- diag(diag( var(Xapp[indk,]) ))
		param$mean[k,] <- apply(Xapp[indk,], MARGIN = 2, mean)
		param$prop[k] <- table(zapp)[k] / length(zapp)
	}

	param
}

ad.val <- function(param, Xtst)
{
	n <- dim(Xtst)[1]
	p <- dim(Xtst)[2]
	g <- length(param$prop)

	out <- NULL
	f1 = mvdnorm(Xtst, params$mean[1,], params$param$MCov[,,1])
    f2 = mvdnorm(Xtst, params$mean[2,], params$param$MCov[,,2])

	prob <- matrix(0, nrow=n, ncol=g)

	for (k in 1:g)
	{
		prob[,k] <- mvdnorm(Xtst, params$mean[k,], params$param$MCov[,,k])
	}
	prob <- prob / ( f1*params$mean[1,] + f2*params$mean[2,] )
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred

	out
}
