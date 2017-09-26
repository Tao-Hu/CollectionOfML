
library(mvtnorm)
library(fields)

# Define the activation function and its derivative

 g  <- function(z){1/(1+exp(-z))}
 gp <- function(z){exp(z)/(1+exp(z))^2}

# Generate a fake dataset

 m <- 25    # The images are m x m.
 n <- 500   # Number of subjects

 p <- m^2
 u <- 1:m
 v <- 1:m
 s <- expand.grid(u,v)
 d <- as.matrix(dist(s))
 C <- exp(-d/4)
 X <- rmvnorm(n,rep(0,p),C)

 image.plot(u,v,matrix(X[1,],m,m),main="Image for subject 1")


# Weights for the first node

 alpha1 <- exp(- ((s[,1]-0.1*m)^2 + (s[,2]-0.2*m)^2)/(2^2))
 alpha1 <- 10*alpha1/sum(alpha1)
 image.plot(u,v,matrix(alpha1,m,m))

# Weights for the second node

 alpha2 <- exp(- ((s[,1]-0.75*m)^2 + (s[,2]-0.8*m)^2)/(3^2))
 alpha2 <- 3*alpha2/sum(alpha2)
 image.plot(u,v,matrix(alpha2,m,m))

# Plot the subjects with the largest and smallest value for each node

 Z1 <- X%*%alpha1
 Z2 <- X%*%alpha2

 smallz1 <- which.min(Z1)
 smallz2 <- which.min(Z2)
 bigz1   <- which.max(Z1)
 bigz2   <- which.max(Z2)

 image.plot(u,v,matrix(X[smallz1,],m,m),main="Image for subject with smallest Z1")
 image.plot(u,v,matrix(X[bigz1,],m,m),main="Image for subject with largest Z1")
 image.plot(u,v,matrix(X[smallz2,],m,m),main="Image for subject with smallest Z2")
 image.plot(u,v,matrix(X[bigz2,],m,m),main="Image for subject with largest Z2")

# Generate the response

 beta <- c(0,2,4)
 plot(Z1,g(Z1))
 Y    <- beta[1] + beta[2]*g(Z1)+ beta[3]*g(Z2) + rnorm(n)/2

# Plot the sample correlation between Y and each X

 r    <- cor(Y,X)
 image.plot(u,v,matrix(r,m))

 
# Define the adjacencies for the graphical ridge penalty
 
  ADJ <- ifelse(d==1,1,0)
  adj1 <- row(ADJ)[ADJ==1]
  adj2 <- col(ADJ)[ADJ==1]
  keep <- adj1<adj2
  adj1 <- adj1[keep]
  adj2 <- adj2[keep]
  M    <- rowSums(ADJ)

# Apply coordinate descent to estimate beta, alpha1, and alpha2

 steps  <- 1000
 gamma  <- 2*seq(0,.0002,.00002) # Step sizes to consider in the line search
 lambda <- 500                   # Ridge penalty

 traceb1 <- rep(0,steps)
 tracea1 <- rep(0,steps)
 tracea2 <- rep(0,steps)

# Initial values

 b  <- c(mean(Y),.1,.1)
 a1 <- rexp(p,100)
 a2 <- rexp(p,100)

 for(t in 1:steps){
  
   
  # Grad for beta
   Z1     <- X%*%a1
   Z2     <- X%*%a2
   gZ     <- cbind(1,g(Z1),g(Z2))
   gr0    <- -2*t(gZ)%*%(Y-gZ%*%b)  

  # Grad for alpha1

   r      <- Y-b[1]-b[3]*g(Z2)
   w      <- -2*(r-b[2]*g(Z1))*b[2]*gp(Z1)    
   gr1    <- as.vector(t(w)%*%X) + 
             2*lambda*(M*a1 - ADJ%*%a1)

  # Grad for alpha2

   r      <- Y-b[1]-b[2]*g(Z1)
   w      <- -2*(r-b[3]*g(Z2))*b[3]*gp(Z2)    
   gr2    <- as.vector(t(w)%*%X) + 
             2*lambda*(M*a2 - ADJ%*%a2)


  # Line search

   obj    <- gamma
   for(j in 1:length(obj)){ # Line search    
      canb   <- b - gamma[j]*gr0
      cana1  <- a1 - gamma[j]*gr1
      cana2  <- a2 - gamma[j]*gr2
      obj[j] <- sum((Y-canb[1] - canb[2]*g(X%*%cana1)-canb[3]*g(X%*%cana2))^2) + 
                lambda*sum((cana1[adj1]-cana1[adj2])^2)+
                lambda*sum((cana2[adj1]-cana2[adj2])^2)
   }
   gam <- gamma[which.min(obj)]
   b   <- b  - gam*gr0
   a1  <- a1 - gam*gr1
   a2  <- a2 - gam*gr2

  # Plot the current value

   if(t %% 10 == 0){
    par(mfrow=c(2,2))
    image.plot(matrix(a1,m),main=t)
    image.plot(matrix(a2,m),main=t)
    plot(gamma,obj)
    plot(b)
   }
 
   traceb1[t] <- b[2]
   tracea1[t] <- a1[10]
   tracea2[t] <- a2[25]
}



# Plot the results

 par(mfrow=c(3,2))
 image.plot(matrix(alpha1,m,m),main="True alpha1")
 image.plot(matrix(a1,m,m),main="Estimated alpha1")
 image.plot(matrix(alpha2,m,m),main="True alpha2")
 image.plot(matrix(a2,m,m),main="Estimated alpha2")
 plot(tracea1)
 plot(tracea2)


