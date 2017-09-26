rm(list=ls())

library(glasso)

n <- 100
p <- 10

# Construct a sparse precision matrix

 trueP <- diag(p)
 for(j in 5:p){
  trueP[j,j-1] <- -.4
  trueP[j-1,j] <- -.4
 }
 trueS <- solve(trueP)
 trueA <- ifelse(trueP!=0 & row(trueP)!=col(trueP),1,0)

 trueP
 trueS
 trueA

 library(network)
 g <- network(trueA)
 plot(g,label=1:p,main="True network")
 
# Generate data

 x <- matrix(rnorm(n*p),n,p)%*%chol(trueS)
 S <- cov(x)
 S

# Compute the tuning parameter using BIC

 nr  <- 100
 rho <- seq(0.1,1,length=nr)
 bic <- rho
 for(j in 1:nr){
  a       <- glasso(S,rho[j])
  p_off_d <- sum(a$wi!=0 & col(S)<row(S))
  bic[j]  <- -2*(a$loglik) + p_off_d*log(n)
 }

 best <- which.min(bic)
 plot(rho,bic)
 points(rho[best],bic[best],pch=19)

# Plot the graph obtained from graph lasso

 a <- glasso(S,rho[best])
 P <- a$wi
 A <- ifelse(P!=0 & row(P)!=col(P),1,0)

 g <- network(A)
 plot(g,label=1:p,main="Estimated network")
 




