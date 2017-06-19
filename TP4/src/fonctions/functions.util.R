library("plyr")
maxN <- function(x, N=2){
    len <- length(x)
    if(N>len){
        warning('N greater than length(x).  Setting N=length(x)')
        N <- length(x)
    }
    sort(x,partial=len-N+1)[len-N+1]
}

minN <- function(x, N=2){
    len <- length(x)
    if(N>len){
        warning('N greater than length(x).  Setting N=length(x)')
        N <- length(x)
    }
    sort(x,partial=len-(len-N))[N]
}

compute.sucess.rate = function(predictedClasses, actualClasses) {
    zvalPredictedKnowingZvalContingency = table(predictedClasses, actualClasses)
    correctPredictions = sum(diag(zvalPredictedKnowingZvalContingency))
    totalPredictions = sum(zvalPredictedKnowingZvalContingency)
    successRate = correctPredictions / totalPredictions
}


plot2DimClasses <- function(file) {
    data <- read.csv(file)
    zIndex = dim(data)[2]
    X <- data[,1:zIndex-1]
    Z <- data[,zIndex]
    class_colors = mapvalues(Z, from = c("1", "2"), to = c("red", "black"))
    class_colors = as.character(class_colors)
    #plot(X, col=c("red", "black")[Z])
    plot(X, col=class_colors)
    params = adq.app(X,Z)
    print(file)
    print("Classe 1")
    print("params$pi[[1]]")
    print(round(params$pi[[1]], 2))
    print("params$mu[[1]]")
    print(round(params$mu[[1]], 2))
    print("params$sigma[[1]]")
    print(round(params$sigma[[1]], 2))
    
    print("Classe 2")
    print("params$pi[[2]]")
    print(round(params$pi[[2]], 2))
    print("params$mu[[2]]")
    print(round(params$mu[[2]], 2))
    print("params$sigma[[2]]")
    print(round(params$sigma[[2]], 2))
}
plotAfterACPClasses <- function(file, centerEnabled = T, scaleEnabled = T) {
    data <- read.csv(file)
    zIndex = dim(data)[2]
    X <- data[,1:zIndex-1]
    Z <- data[,zIndex]
    acp = prcomp(X, center = centerEnabled, scale = scaleEnabled)
    plot(acp$x[,1:2], col=Z)
}