
# Simulate a fake dataset

 n         <- 50
 p         <- 20
 beta      <- rep(0,p)
 beta[1:5] <- 1:5/5

 X <- matrix(rnorm(n*p),n,p)
 X <- scale(X) 

 Xb <- X%*%beta
 Y <- X%*%beta+rnorm(n)
 Y <- Y-mean(Y)


# Fit the lasso with lars

 library(lars)
 lasso  <- lars(X,Y)
 plot(lasso)

# Match the solution with coordinate descent



 nl      <- 200
 lambda  <- seq(0,200,length=nl)

 beta_cd <- matrix(0,nl,p)

 for(l in 1:nl){

   # Initial values
   b <- rep(0,p)
   r <- Y - X%*%b

   # Coordiante descent
   for(step in 1:50){for(j in 1:p){

     # partial residuals
     r <- r + X[,j]*b[j]
   
     # soft-threshold solution
     xr <- sum(X[,j]*r)
     xx <- sum(X[,j]^2)   
     b[j] <- (abs(xr)-lambda[l]/2)/xx
     b[j] <- sign(xr)*ifelse(b[j]>0,b[j],0)
   
     # residuals
     r <- r - X[,j]*b[j]
   
   }}
   
   beta_cd[l,]<-b
}


# Plot the solution path

pct <- rowSums(abs(beta_cd))/sum(abs(beta_cd[1,]))

matplot(pct,beta_cd,type="l",lty=1,
        xlab="|beta|/max(|beta|)",ylab="Coefficients")
text(1.02,beta_cd[1,],1:p,cex=1,col=1:p)







#out.ls=lm(y~xs)                      # ols fit on standardized
#beta.ols=out.ls$coeff[2:(m+1)]       # ols except for intercept
#w=abs(beta.ols)                      # weights for adaptive lasso
#xs=scale(xs,center=FALSE,scale=1/w)  # xs times the weights
#object=lars(xs,y,type="lasso",normalize=FALSE)




