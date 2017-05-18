front.kppv <- function(Xapp, zapp, K, discretisation=50)
{
    deltaX <- (max(Xapp[,1]) -min(Xapp[,1]))/discretisation
    deltaY <- (max(Xapp[,2]) -min(Xapp[,2]))/discretisation
    minX <- min(Xapp[,1])-deltaX
    maxX <- max(Xapp[,1])+deltaX
    minY <- min(Xapp[,2])-deltaY
    maxY <- max(Xapp[,2])+deltaY

    # grille d'affichage 
    grilleX <- seq(from=minX,to=maxX,by=deltaX)
    naffX <- length(grilleX)
    grilleY <- seq(from=minY,to=maxY,by=deltaY)
    naffY <- length(grilleY)
    grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))

    # calcul des valeurs de la fonction 
    valf <- kppv.val(Xapp, zapp, K, grille)
    plot(Xapp, col=c("red","green","blue","magenta","orange")[zapp], asp=1)
    contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}
