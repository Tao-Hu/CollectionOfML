
# Singular value decomposition (SVD) for data compression


## Generate data with two prominent eigenvectors

 library(fields)
 p <- 1000  # number of variables
 n <- 100   # number of samples

 t <- seq(0,2*pi,length=p)
 X <- matrix(0,n,p)

 for(i in 1:n){
  X[i,] <- 5 + rnorm(1)*sin(t) + 0.5*rnorm(1)*cos(2*t) + rnorm(p)/20
 }
 X <- sweep(X,2,colMeans(X),"-")
 matplot(t(X),type="l")

## Compute the sample ($p \times p$) covariance of $X$


 S <- cov(X)

 image.plot(1:p,1:p,S,
           xlab="Covariate number",
           ylab="Covariate number",
           main="Sample covariance")


## FACT: If $X$ is centered then $S = X^tX/(n-1)$

 tXX <- t(X)%*%X/(n-1)
 S[1:3,1:3]
 tXX[1:3,1:3]


## Compute the eigen decomposition of S

 tick <- proc.time()[3]
 e    <- eigen(S)
 tock <- proc.time()[3]
 tock-tick

 # Cleary only two eigenvectors are needed

 lam  <- e$values
 lam[1:5]
 plot(lam,xlab="j",ylab=expression(lambda[j]),main="Eigenvalues")
 plot(cumsum(lam)/sum(lam),ylab="Proportion of variance explained")


## Plot the two eigenvectors

 EV2 <- e$vectors[,1:2]
 matplot(EV2,type="l")

## Plot the reconstructed covariance matrix


 Shat <- lam[1]*outer(EV2[,1],EV2[,1]) + lam[2]*outer(EV2[,2],EV2[,2])

 image.plot(1:p,1:p,S,
           xlab="Covariate number",
           ylab="Covariate number",
           main="Sample covariance")


 image.plot(1:p,1:p,Shat,
           xlab="Covariate number",
           ylab="Covariate number",
           main="Rank 2 covariance")

## SVD is equivalent and faster than eigen when p>n.  


 tick <- proc.time()[3]
 SVD  <- svd(X)
 tock <- proc.time()[3]
 tock-tick

 names(SVD)
 dim(SVD$u)
 dim(SVD$v)
 length(SVD$d)

 SVD2 <- SVD$v[,1:2]
 matplot(SVD2,type="l")



## Data compression from rank $p$ to rank 2

 dim(X)
 Xcomp <- X%*%SVD2
 dim(Xcomp)

## Reconstruction for observations 1-5

 for(i in 1:5){

   Xi   <- SVD2[,1]*Xcomp[i,1] + SVD2[,2]*Xcomp[i,2]
   plot(X[i,],type="l",main=paste("Observation",i))
   lines(Xi,col=2,lwd=2)
   legend("topleft",c("Full","Reconstructed"),lwd=2,inset=0.01,col=1:2)
 }
