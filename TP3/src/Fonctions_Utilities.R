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