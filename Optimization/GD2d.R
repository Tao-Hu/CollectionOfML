library(fields)

# Define the log likelihood (simple logistic regression)

 f <- function(beta,y,x){
   xb <- beta[1]+x*beta[2]
   ll <- y*xb - log(1 + exp(xb))     
 return(sum(ll))}

# Gradient 
 fp <- function(beta,y,x){
   xb <- beta[1]+x*beta[2]
   l1 <-   y - exp(xb)/(1 + exp(xb))     
   l2 <- x*y - x*exp(xb)/(1 + exp(xb))     
    g <- c(sum(l1),sum(l2))
 return(g)}

# Generate a fake dataset from simple logistic regression

 n    <- 1000
 beta <- c(-.5,0.5)
  
# set.seed(0820)

 x    <- rbinom(n,1,.5)
 y    <- rbinom(n,1,1/(1+exp(-beta[1]-x*beta[2]))) 

# Plot the log-likelihood and its derivative:

 nb <- 50
 b1 <- seq(-1,0,length=nb)
 b2 <- seq( 0,1,length=nb)

 ll <- matrix(0,nb,nb)
 for(i in 1:nb){for(j in 1:nb){
    ll[i,j] <- f(c(b1[i],b2[j]),y,x)
 }}
 
 image.plot(b1,b2,ll,xlab=expression(beta[1]),ylab=expression(beta[2]),col=gray(1-seq(0,0.5,.05)))

# One solution for gradient ascent & GA + momemtum

 steps     <- 25
 init      <- c(-1,0)

 # gradient ascent:
  stepsize  <- 0.002
  b_ga      <- matrix(init,2,steps,byrow=F)
 
  points(b_ga[1,1],b_ga[2,1],pch=19)
  for(step in 2:steps){
    b_ga[,step] <- b_ga[,step-1] + stepsize*fp(b_ga[,step-1],y,x)
    points(b_ga[1,step],b_ga[2,step],pch=19,col=2)
  } 
  lines(b_ga[1,],b_ga[2,],col=2)

# GA + M

 stepsize1 <- 0.001
 stepsize2 <- 0.7
 b_gam     <- matrix(init,2,steps,byrow=F)

 for(step in 3:steps){
    b_gam[,step] <- b_gam[,step-1] + 
                    stepsize1*fp(b_gam[,step-1],y,x)+
                    stepsize2*(b_gam[,step-1]-b_gam[,step-2])
    points(b_gam[1,step],b_gam[2,step],pch=19,col=3)
 } 
 lines(b_gam[1,],b_gam[2,],col=3)


# Coordinate ascent

 stepsize1 <- 0.001
 stepsize2 <- 0.001
 b_ca     <- matrix(init,2,steps,byrow=F)

 for(step in 2:steps){
    b_ca[,step] <- b_ca[,step-1]

    # first direction
    for(rep in 1:20){
      b_ca[1,step] <- b_ca[1,step] + 
                      stepsize1*fp(b_ca[,step],y,x)[1]
    }
    points(b_ca[1,step],b_ca[2,step],col=4)

    # second direction
    for(rep in 1:20){
      b_ca[2,step] <- b_ca[2,step] + 
                      stepsize2*fp(b_ca[,step],y,x)[2]
    }
    points(b_ca[1,step],b_ca[2,step],pch=19,col=4)
 } 
 lines(b_ca[1,],b_ca[2,],col=4)


 legend("topright",c("GA","GA+M","CA"),col=2:4,lty=1,pch=19,inset=0.05,bg=gray(1))