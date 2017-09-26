
# Variable screening for the homes data


# Extract the variables

 load("S:\\Documents\\www\\BigData\\code\\homes.RData")
 Y   <- homes[,7] # Precip
 n   <- length(Y)

 # Abundance

  Abd   <- as.matrix(OTU)

 # Relative abundance (i.e., the proportion of the total for each house)

  RA    <- sweep(Abd,1,rowSums(Abd),"/")

 # Occupancy 

  Occ   <- ifelse(Abd>0,1,0)
  vvv   <- apply(Occ,2,var)
  Occ   <- Occ[,vvv>0]          # Remove OTUs that are all 0 or all 1
 
 hist(Y)
 hist(Abd)
 hist(RA)
 hist(Occ)

## Five-fold CV

 # Number of variables to keep

  q    <- seq(2,200,5)
  nq   <- length(q)

 # Split the observations into 5 folds

  fold <- sample(1:5,n,replace=TRUE)

  table(fold)

 # Store the results

  MSE   <- matrix(0,nq,3)

  colnames(MSE) <- c("Abd","Rel Abd","Occ")
  rownames(MSE) <- paste("q =",q,"variables")

 for(j in 1:nq){

  Yhat1 <- rep(0,n)
  Yhat2 <- rep(0,n)
  Yhat3 <- rep(0,n)

  for(f in 1:5){ 

    Ytrain <- Y[fold!=f]

   # Abundance

    Xtrain <- Abd[fold!=f,]
    Xtest  <- Abd[fold==f,]
   
    r      <- abs(cor(Ytrain,Xtrain))
    keep   <- which(rank(-r) <= q[j])

    ols    <- lm(Ytrain~Xtrain[,keep])$coef
    y      <- ols[1] + Xtest[,keep]%*%ols[-1]
   
    Yhat1[fold==f] <- y  

   # Relative abundance

    Xtrain <- RA[fold!=f,]
    Xtest  <- RA[fold==f,]
   
    r      <- abs(cor(Ytrain,Xtrain))
    keep   <- which(rank(-r) <= q[j])

    ols    <- lm(Ytrain~Xtrain[,keep])$coef
    y      <- ols[1] + Xtest[,keep]%*%ols[-1]
   
    Yhat2[fold==f] <- y  

   # Ocuupancy

    Xtrain <- Occ[fold!=f,]
    Xtest  <- Occ[fold==f,]
   
    r      <- abs(cor(Ytrain,Xtrain))
    keep   <- which(rank(-r) <= q[j])

    ols    <- lm(Ytrain~Xtrain[,keep])$coef
    y      <- ols[1] + Xtest[,keep]%*%ols[-1]
   
    Yhat3[fold==f] <- y  


  }

  MSE[j,1] <- mean((Y-Yhat1)^2)      
  MSE[j,2] <- mean((Y-Yhat2)^2)      
  MSE[j,3] <- mean((Y-Yhat3)^2)      

 }



# Compare the results

#Occupancy is clearly superior to abundance, and it appears that about 100 predictors is sufficient.

  matplot(q,MSE,type="l",lty=1,xlab="Number of covariates included",ylab="MSE")
  legend("topleft",colnames(MSE),inset=0.01,col=1:3,lty=1)

  plot(q,MSE[,3],xlab="Number of covariates included",ylab="MSE using occupancy")


# Fit the final model using least squares

   
    r      <- abs(cor(Y,Occ))

    hist(r)

    keep   <- which(rank(-r) <= 100)
    ols    <- lm(Y~Occ[,keep])
    
    plot(ols,ask=FALSE)
    summary(ols)

