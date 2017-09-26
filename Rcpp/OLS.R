require(Rcpp)
require(inline)
require(RcppArmadillo)

# Function to compute the least squares solution in C

ols_code = "

  arma::colvec y = Rcpp::as<arma::colvec> (y_);
  arma::mat    X = Rcpp::as<arma::mat>(X_) ;
  
  arma::colvec beta = arma::inv(arma::trans(X)*X) * arma::trans(X) * arma::colvec(y);

  return Rcpp::wrap( beta );
"

ols <- cxxfunction(signature(y_="numeric", X_="matrix"),
                   body=ols_code, 
                   plugin="RcppArmadillo")

#Generate a fake dataset

 n         <- 100
 X         <- cbind(1,rnorm(n))
 y         <- 1*X[,2] + rnorm(n)

# Compute the least squares solution using lm and rcpp

 beta.lm   <- lm(y~X-1)$coef
 beta.rcpp <- ols(y,X)

 print(beta.lm)
 print(beta.rcpp)
