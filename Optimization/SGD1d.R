

# Define the log likelihood (simple logistic regression)

 f <- function(y,x,beta){
   sum(y*x*beta) - sum(log(1 + exp(x*beta)))     
 }

# Gradient 

 fp <- function(y,x,beta){
   sum(y*x) - sum(x*exp(x*beta)/(1 + exp(x*beta)))     
 }

# Generate a fake dataset from simple logistic regression

 nt   <- 10000 # number of time points
 m    <- 100   # observations per time point
 beta <- 0.75  # True value
  
# SGA tuning

 stepsize  <- 5
 power     <- 1
 init      <- 0.5
 b         <- rep(init,nt)
 
# Streaming data and SGA updating

  set.seed(0820)
  for(t in 2:nt){


    # Generate data at this time point
    x    <- rbinom(m,1,.01)
    y    <- rbinom(m,1,1/(1+exp(-x*beta))) 

    # SGA using only data from this time point
    gamma <- stepsize/t^power
    b[t]  <- b[t-1] + gamma*fp(y,x,b[t-1])
  } 

# Plot the results

  par(mfrow=c(1,1))
  plot(b,type="l",xlab="Time (t)",ylab="Estimate (b)")
  abline(beta,0)



