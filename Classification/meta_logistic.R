##########################################################
# This code is the solution for computing assignment #7  #
##########################################################

# Simulation set up

 sims <- 100   # Number of datasets
 n    <- 1000  # Batch size
 p    <- 51
 beta <- seq(0,1,length=p)/10

# Storage matrices

 beta1 <- matrix(0,sims,p+1)
 beta2 <- matrix(0,sims,p+1)
 nb    <- rep(0,sims)

# GO!

for(sim in 1:sims){

 # first batch

 X <- matrix(rnorm(n*p),n,p)
 Y <- rbinom(n,1,1/(1+exp(-X%*%beta)))

 allX <- X
 allY <- Y

 fit <- glm(Y~X,family="binomial")
 b   <- fit$coef
 Q   <- solve(summary(fit)$cov.unscaled)
 Qb  <- Q%*%b

 # Determine the number of batches

 se      <- summary(fit)$coef[,2]
 maxse   <- max(se[-1])
 nb[sim] <- ceiling((maxse/0.01)^2)

 # Collect remaining data

 if(nb[sim]>1){for(batch in 2:nb[sim]){
    X <- matrix(rnorm(n*p),n,p)
    Y <- rbinom(n,1,1/(1+exp(-X%*%beta)))

    allX <- rbind(allX,X)
    allY <- c(allY,Y)
   
    fit    <- glm(Y~X,family="binomial")
    b_temp <- fit$coef
    Q_temp <- solve(summary(fit)$cov.unscaled)
    Q      <- Q + Q_temp
    Qb     <- Qb + Q_temp%*%b_temp
 }}

 # Meta analysis estimate
 beta2[sim,] <- as.vector(solve(Q)%*%Qb) 
 
 # Full likelihood estimate
 beta1[sim,] <- glm(allY~allX,family="binomial")$coef

}

# Display the number of batches per dataset
table(nb)

# Plot the sampling distribution of each estimator and 
# its MSE

MSE1 <- mean(sweep(beta1[,-1],2,beta,"-")^2)
MSE2 <- mean(sweep(beta2[,-1],2,beta,"-")^2)
MSE1 <- round(1000*MSE1,5)
MSE2 <- round(1000*MSE2,5)

par(mfrow=c(2,1))
boxplot(beta1[,-1],main=paste("Full likelihood - 1000*MSE =",MSE1))
lines(beta,lwd=2,col=2)
boxplot(beta2[,-1],,main=paste("Meta likelihood - 1000*MSE =",MSE2))
lines(beta,lwd=2,col=2)


