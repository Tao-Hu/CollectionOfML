
library(splines)

#Generate data

 set.seed(0820)

 n  <- 100
 x  <- seq(0,1,length=n)
 fx <- 10*x*sin(10*pi*x) + 3*cos(2*pi*x)
 y  <- rnorm(n,fx,1)

 plot(x,y)
 lines(x,fx)

 legend("topleft",c("Data","True curve"),
                  lty=c(NA,1),pch=c(1,NA),inset=0.05)

# Plot the b-spline basis functions

 df <- 10
 B  <- bs(x,df)
 matplot(B,type="l",lty=1)

# Fit splines using OLS

 fit <- lm(y~B)
 fhat <- fit$fitted
 summary(fit)
 plot(x,y)
 lines(x,fx)
 lines(x,fhat,col=2)

 legend("topleft",c("Data","True curve","Estimated curve"),
                  lty=c(NA,1,1),pch=c(1,NA,NA),col=c(1,1,2),inset=0.05)

# Fit splines using OLS and pick the number of basis functions using BIC
    
 df  <- seq(5,90,1)
 R2  <- df
 aR2 <- df
 BIC <- df
 for(j in 1:length(df)){
  B      <- bs(x,df[j])
  fit    <- lm(y~B)
  R2[j]  <- summary(fit)$r.squared
  aR2[j] <- summary(fit)$adj.r.squared
  BIC[j] <- BIC(fit)
 }
 plot(df,R2,type="l",ylim=0:1)
 lines(df,aR2,col=2)
 legend("bottomright",c("R2","Adjusted R2"),
                  lty=1,col=1:2,inset=0.05)

 plot(df,BIC)

# Fit a spline model using 50 basis functions and ridge regression
 
 B      <- cbind(1,bs(x,50))
 p      <- ncol(B)
 D      <- 2*diag(p)
 D[1,1] <- 0
   
 BB     <- t(B)%*%B
 lambda <- .1
 H      <- solve(BB+lambda*D)%*%t(B)
 bhat   <- H%*%y
 fhat   <- B%*%bhat
 effpar <- sum(diag(B%*%H))     

 plot(x,y,main=paste("Effective number of parameters =",round(effpar,1)))
 lines(x,fhat)
   
# A note on sparse matrices

 B        <- bs(x,1000)   
 BB       <- t(B)%*%B
 diag(BB) <- diag(BB) + 1
 tBy      <- t(B)%*%y
   
 BB[1:20,1:20] # It is sparse!

 library(spam)
 BBspam <- as.spam(BB)

 system.time(b1 <- solve(BB)%*%tBy)
 system.time(b2 <- solve(BBspam)%*%tBy)
 system.time(b3 <- solve(BB,tBy))
 system.time(b4 <- solve(BBspam,tBy))

 b1[1:10]
 b2[1:10]
 b3[1:10]
 b4[1:10]





   



 


