
library(kernlab)

# First a quadratic example:

 n <- 1000
 x <- cbind(runif(n),runif(n))
 y <- ifelse(x[,2] > x[,1]^2,1,-1)

 xp <- cbind(runif(2*n),runif(2*n))
 yp <- ifelse(xp[,2] > xp[,1]^2,1,-1)

 y   <- as.factor(y)
 yp  <- as.factor(yp)
 x1  <- x
 z1  <- xp
 x2  <- cbind(x,x^2,x[,1]*x[,2])
 z2  <- cbind(xp,xp^2,xp[,1]*xp[,2])


 plot(x,col=ifelse(y=="1",1,3),pch=19,main="Training data")
 legend("topright",c("Y=1","Y=-1"),pch=19,col=c(1,3),inset=0.05,bg=gray(1),cex=1.5)

 # Vanilldadot gives a linear SVM
 fit1<-ksvm(x1,y,kernel="vanilladot")
 fit2<-ksvm(x2,y,kernel="vanilladot")

 SV <- x[alphaindex(fit2)[[1]],]
 plot(SV,pch=19,main="Locations of the support vectors")


 # Predictions:

 yhat1 <- predict(fit1,z1)
 yhat2 <- predict(fit2,z2)
 table(yp,yhat1)
 table(yp,yhat2)

 plot(xp,col=ifelse(yhat1=="1",1,3),pch=19,main="Testing data - linear model")
 legend("topright",c("Y=1","Y=-1"),pch=19,col=c(1,3),inset=0.05,bg=gray(1),cex=1.5)

 plot(xp,col=ifelse(yhat2=="1",1,3),pch=19,main="Testing data - quadratic model")
 legend("topright",c("Y=1","Y=-1"),pch=19,col=c(1,3),inset=0.05,bg=gray(1),cex=1.5)






# First a harder example:

 n    <- 1000
 x    <- cbind(runif(n),runif(n))
 eta  <- x[,2] - sin(10*x[,1])
 prob <- 1/(1+exp(-5*eta))
 y    <- 2*rbinom(n,1,prob)-1

 xp   <- cbind(runif(n),runif(n))
 eta  <- xp[,2] - sin(10*xp[,1])
 prob <- 1/(1+exp(-5*eta))
 yp   <- 2*rbinom(n,1,prob)-1

 y  <- as.factor(y)
 yp <- as.factor(yp)

 plot(x,col=ifelse(y=="1",1,3),pch=19,main="Training data")
 legend("topright",c("Y=1","Y=-1"),pch=19,col=c(1,3),inset=0.05,bg=gray(1),cex=1.5)


 # Non-linear SVM with Gaussian (rbf = radial basis function) kernel
 fit<-ksvm(x,y,kernel="rbfdot")

 SV <- x[alphaindex(fit)[[1]],]
 plot(SV,pch=19,main="Locations of the support vectors")


 # Predictions:

 yhat <- predict(fit,xp)
 table(yp,yhat)

 plot(xp,col=ifelse(yhat=="1",1,3),pch=19,main="Testing data")
 legend("topright",c("Y=1","Y=-1"),pch=19,col=c(1,3),inset=0.05,bg=gray(1),cex=1.5)









