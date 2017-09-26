
# Principal component regression (PCR) for the homes data

## Load the data

 load("S:\\Documents\\www\\BigData\\code\\homes.RData")

 Y  <- homes[,7] # Precip
 n  <- length(Y)

 X  <- as.matrix(OTU)
 X  <- ifelse(X>0,1,0)
 v  <- apply(X,2,var)
 X  <- X[,v>0]
 X  <- scale(X)  #Now each columns has mean zero and variance one


## PCA

  e   <- eigen(cov(X))
  lam <- e$values
  pct <- cumsum(lam)/sum(lam)

  plot(lam,xlab="j",ylab=expression(lambda[j]))
  plot(pct,xlab="j",ylim=0:1,ylab="Percent variation explained")
  abline(0.50,0)
  abline(0.80,0)
  abline(0.90,0)
  abline(0.95,0)

  plot(e$vector[,1],main="The first eigenvector")
  plot(e$vector[,2],main="The second eigenvector")



## Construct new covariates

  Z <- X%*%e$vector
  Z[1,1:3]
  sum(X[1,]*e$vector[,1])
  sum(X[1,]*e$vector[,2])
  sum(X[1,]*e$vector[,3])


## Determine how many terms we need to explain 50%, 80%, 90% and 95% of the variation

  n50 <- which.min(abs(pct-0.50))
  n50

  n80 <- which.min(abs(pct-0.80))
  n80

  n90 <- which.min(abs(pct-0.90))
  n90

  n95 <- which.min(abs(pct-0.95))
  n95

## Fit the regressions

  fit50 <- lm(Y~Z[,1:n50])
  fit80 <- lm(Y~Z[,1:n80])
  fit90 <- lm(Y~Z[,1:n90])
  fit95 <- lm(Y~Z[,1:n95])

  summary(fit50)

  summary(fit50)$adj.r.squared
  summary(fit80)$adj.r.squared
  summary(fit90)$adj.r.squared
  summary(fit95)$adj.r.squared


