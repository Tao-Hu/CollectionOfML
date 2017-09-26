
library(fields)
p <- 1000
n <- 2000
t <- seq(0,2*pi,length=p)
X <- matrix(0,n,p)

for(i in 1:n){
  X[i,] <- 5 + rnorm(1)*sin(t) + 0.5*rnorm(1)*cos(2*t) + rnorm(p)/5
}
X <- sweep(X,2,colMeans(X),"-")


tick <- proc.time()[3]
SVD <- svd(X)$v[,1:2]
tock <- proc.time()[3]
tock-tick

matplot(SVD,type="l")


nsamps <- 10
size   <- 20

tick <- proc.time()[3]

sub  <- rank(runif(n))<=size
aSVD <- svd(X[sub,])$v[,1:2]/nsamps

for(j in 2:nsamps){
 sub     <- rank(runif(n))<=size
 ev2     <- svd(X[sub,])$v[,1:2]

 ev2[,1] <- sign(cor(ev2[,1],SVD[,1]))*ev2[,1]
 ev2[,2] <- sign(cor(ev2[,2],SVD[,2]))*ev2[,2]
 
 aSVD    <- aSVD+ev2/nsamps
}
tock <- proc.time()[3]
tock-tick

matplot(SVD,type="l",col=1)
matplot(aSVD,type="l",add=TRUE,col=2)
legend("topleft",c("True","Approx"),col=1:2,lwd=1,inset=0.05)




