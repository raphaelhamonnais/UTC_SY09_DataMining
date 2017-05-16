front.ceuc <- function(mu, Xaff, zaff, discretisation=50)
{
    deltaX <- (max(Xaff[,1]) -min(Xaff[,1]))/discretisation
    deltaY <- (max(Xaff[,2]) -min(Xaff[,2]))/discretisation
    minX <- min(Xaff[,1])-deltaX
    maxX <- max(Xaff[,1])+deltaX
    minY <- min(Xaff[,2])-deltaY
    maxY <- max(Xaff[,2])+deltaY

    # grille d'affichage 
    grilleX <- seq(from=minX,to=maxX,by=deltaX)
    naffX <- length(grilleX)
    grilleY <- seq(from=minY,to=maxY,by=deltaY)
    naffY <- length(grilleY)
    grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))

    # calcul des valeurs de la fonction 
    valf <- ceuc.val(mu, grille)
    plot(Xaff, col=c("red","green","blue","magenta","orange")[zaff], asp=1)
    contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}
