require(Rcpp)
require(inline)
require(RcppArmadillo)


# Function to perform the parametric bootstrap for linear regression

boot_code = "

  arma::colvec y = Rcpp::as<arma::colvec> (y_);
  arma::mat    X = Rcpp::as<arma::mat>(X_) ;

  int p = X.n_cols;
  int n = X.n_rows;
  int m = as<int>(m_);

  arma::mat    H   = arma::inv(arma::trans(X)*X) * arma::trans(X);
  arma::colvec b   = H*y;
  arma::colvec Xb  = X*b;
  arma::colvec res = y-Xb;

  double sig2 = arma::as_scalar( arma::trans(res)*res/(n-p) );
  double sig  = sqrt(sig2);
    
  NumericMatrix beta(m,p);
  NumericVector bb(p);
    
  for (int i = 0; i < m; i++){  
    res       = rnorm(n,0,sig); 
    bb        = H*(Xb + res);
    beta(i,_) = bb;
  }
  return Rcpp::wrap(beta);
"

boot <- cxxfunction(signature(y_="numeric", X_="matrix",m_="integer"),
                    body=boot_code, 
                    plugin="RcppArmadillo")

   

# Timing experiment

 n    <- 10^2
 reps <- 10^5

 time        <- rep(0,2)
 names(time) <- c("R","Rcpp")

 X <- cbind(1,rnorm(n))
 y <- 10*X[,2] + rnorm(n,0,5)

# Bootstrap using R

 tick      <- proc.time()[3]
 beta1     <- matrix(0,reps,2)
 H         <- solve(t(X)%*%X)%*%t(X) 
 b         <- H%*%y
 Xb        <- X%*%b
 res       <- y-Xb
 sig       <- sqrt(sum(res^2)/(n-2))
 for(rep in 1:reps){
   y_rep       <- rnorm(n,Xb,sig)
   beta1[rep,] <- H%*%y_rep
 }
 tock      <- proc.time()[3]
 time[1]   <- tock-tick

# Bootstrap using Rcpp

 tick      <- proc.time()[3]
 beta2     <- boot(y,X,reps)
 tock      <- proc.time()[3]
 time[2]   <- tock-tick


summary(lm(y~X-1))$coef

apply(beta1,2,mean)
apply(beta1,2,sd)

apply(beta2,2,mean)
apply(beta2,2,sd)

time
time/time[2]


