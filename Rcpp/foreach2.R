

library(parallel)
library(foreach)
library(doParallel)

# Find out how many cores are available (if you don't already know)

 ncores <- detectCores()

# Create cluster with desired number of cores
 
 cl <- makeCluster(ncores)

# Register cluster

 registerDoParallel(cl)


# Compute a determinant 2 ways

 det_AR <- function(n,rho){
   s <- 1:n
   d <- as.matrix(dist(s))
   C <- rho^d
 return(determinant(C)$modulus[1])}

 n   <- 1000
 m   <- 20
 rho <- seq(0,0.9,length=m)

 output      <- matrix(0,m,2) 
 time        <- rep(0,2)
 names(time) <- c("loop","for each")

 tick <- proc.time()[3]
 for(i in 1:m){
   output[i,1] <- det_AR(n,rho[i])
 }
 tock    <- proc.time()[3]
 time[1] <- tock-tick

 tick       <- proc.time()[3]
 out        <- foreach(a=rep(n,m),b=rho) %dopar% det_AR(a,b)
 output[,2] <- unlist(out)
 tock       <- proc.time()[3]
 time[2]    <- tock-tick

 print(time)
 print(time[1]/time[2])


