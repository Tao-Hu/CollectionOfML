

# Define the function to maximize

 f <- function(x,p=0.8){
      p*exp(-20*(x-0.3)^2) + 
  (1-p)*exp(-100*(x-0.7)^2)
 }

# Gradient 
 fp <- function(x,p=0.8){
      -p*40*(x-0.3)*exp(-20*(x-0.3)^2) -
   (1-p)*200*(x-0.7)*exp(-100*(x-0.7)^2)
 }

# Plot the tangent line

 tanline <- function(x,p=0.8,length=.05,col=2,lwd=2){
   lines(    x  + length*c(-1,1), 
         f(x,p) + length*fp(x,p)*c(-1,1),
         col=col,lwd=lwd)
 }

# Plot the function and its derivative:

 par(mfrow=c(2,1))
 x <- seq(0,1,length=1000)
 plot(x,f(x),type="l")
 plot(x,fp(x),type="l")
 abline(0,0)


# One solution for gradient aschent & GA + momemtum

 steps     <- 25
 init      <- 0.8

 par(mfrow=c(2,2))

 # gradient descent:
  stepsize  <- 0.025
  X         <- rep(init,steps)
 
  plot(x,f(x),type="l")

  points(X[1],f(X[1]),col=3,pch=19)
  tanline(X[1])

  for(step in 2:steps){
    X[step] <- X[step-1] + stepsize*fp(X[step-1])
    points(X[step],f(X[step]),col=3,pch=19)
    tanline(X[step])
  } 

  points(X[step],f(X[step]),col=4,pch=19)
  tanline(X[step],col=4)

  plot(X,ylim=0:1,xlab="Iteration number",ylab="Estimate")
  lines(X)

# GA + M

 stepsize1 <- 0.05
 stepsize2 <- 0.5
 X         <- rep(init,steps)

 plot(x,f(x),type="l")

 points(X[1],f(X[1]),col=3,pch=19)
 tanline(X[1])

 for(step in 3:steps){
   X[step] <- X[step-1] + 
              stepsize1*fp(X[step-1]) + 
              stepsize2*(X[step-1]-X[step-2])
   points(X[step],f(X[step]),col=3,pch=19)
   tanline(X[step])
 } 

 points(X[step],f(X[step]),col=4,pch=19)
 tanline(X[step],col=4)

 plot(X,xlab="Iteration number",ylab="Estimate",ylim=0:1)
 lines(X)






# Try several initial values and see how each performs

 inits <- seq(0.05,0.95,length=50)
 ni    <- length(inits)
 final <- matrix(0,ni,2)
 steps <- 100

 colnames(final) <- c("GD","GD+M")
 rownames(final) <- paste("Init =",inits)

 stepsize  <- 0.05
 stepsize1 <- 0.05
 stepsize2 <- 0.5

 for(i in 1:ni){
   # AD
   X <- rep(inits[i],steps)
   for(step in 2:steps){
     X[step] <- X[step-1] + stepsize*fp(X[step-1])
   } 
   final[i,1]<-X[step]

   # GA + M
   X <- rep(init,steps)
   for(step in 3:steps){
     X[step] <- X[step-1] + 
                stepsize1*fp(X[step-1]) + 
                stepsize2*(X[step-1]-X[step-2])
   } 
   final[i,2]<-X[step]
  }

  par(mfrow=c(1,1))
  matplot(inits,final,type="l",xlab="Initial value",ylab="Final value")
  legend("topleft",c("GA","GA+Mom"),col=1:2,lty=1,inset=0.05)

