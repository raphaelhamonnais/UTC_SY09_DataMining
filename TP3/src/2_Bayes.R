
fSynt1 = function(x) {
    return(-2*x - 3/2)
}
fSynt2 = function(x){
    -4*x*x + 34*x -41 + 5*log(5)
}

fSynt2_b = function(x){
    -fSynt2()
}

data = read.csv("data/Synth1-1000.csv")
X = data[,1:2]
z = factor(data[,3])
plot(X, col = c("red", "black")[z])
curve(fSynt1, from=-4, to=4, ylim=c(-3,3), xlab="x1", ylab="x2", col="blue",lwd=2, add = T)





data = read.csv("data/Synth2-1000.csv")
X = data[,1:2]
z = factor(data[,3])
plot(X, col = c("red", "black")[z])
curve(fSynt2, from=-8, to=6, ylim=c(-1,2), xlab="x1", ylab="x2", col="blue",lwd=2, add = T)
