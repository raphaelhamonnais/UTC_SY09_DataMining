separ2 <- function(X, z)
{
	g <- max(z)

	Xapp <- NULL
	zapp <- NULL
	Xval <- NULL
	zval <- NULL
	Xtst <- NULL
	ztst <- NULL

	for (k in 1:g)
	{
	    indk <- which(z==k)
    	ntot <- length(indk)
	    napp <- round(ntot/2)
		nval <- round(ntot/4)
    	ntst <- ntot-napp-nval

	    itot <- sample(indk)
    	iapp <- itot[1:napp]
    	ival <- itot[(napp+1):(napp+nval)]
	    itst <- itot[(napp+nval+1):ntot]

    	Xapp <- rbind(Xapp, X[iapp,])
	    zapp <- c(zapp, z[iapp])
    	Xval <- rbind(Xval, X[ival,])
	    zval <- c(zval, z[ival])
    	Xtst <- rbind(Xtst, X[itst,])
	    ztst <- c(ztst, z[itst])
	}

	res <- NULL
	res$Xapp <- Xapp
	res$zapp <- zapp
	res$Xval <- Xval
	res$zval <- zval
	res$Xtst <- Xtst
	res$ztst <- ztst

	res
}
