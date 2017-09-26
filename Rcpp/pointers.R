library(inline)
library(Rcpp)

# Rcpp function to multiply a matrix by a creating a new matrix

 mult1_code = "

  Rcpp::NumericMatrix xcpp(x);
  int nr = xcpp.nrow();
  int nc = xcpp.ncol();
  double kcpp = Rcpp::as<double>(k);

  Rcpp::NumericMatrix xnew(nr, nc);

  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      xnew(i,j) = xcpp(i,j);
    }
  }

  for (int i = 0; i < nr; i++){
    for (int j = 0; j < nc; j++){
        xnew[nr * j + i] *= kcpp;
    }
  }

  return xnew;
  "

 mult1 <- cxxfunction(signature(x = "numeric",k = "numeric"), 
                      body=mult1_code, 
                      plugin="Rcpp")




# Rcpp function to multiply a matrix by a constant using a pointer

 mult2_code = "

  Rcpp::NumericMatrix xcpp(x);
  int nr = xcpp.nrow();
  int nc = xcpp.ncol();
  double kcpp = Rcpp::as<double>(k);

  for (int i = 0; i < nr; i++){
    for (int j = 0; j < nc; j++){
        xcpp[nr * j + i] *= kcpp;
    }
  }
  "

 mult2 <- cxxfunction(signature(x = "numeric",k = "numeric"), 
                      body=mult2_code, 
                      plugin="Rcpp")


# Timing experiment

 n     <- 10^2
 pow   <- 10^5
 k     <- k <- exp(log(3)/pow)
 mat   <- diag(n)+1
  
 time        <- rep(0,2)
 names(time) <- c("new matrix","pointer")

 tick    <- proc.time()[3]
 mat1    <- mat
 for(rep in 1:pow){
   mat1 <- mult1(mat1,k)
 }
 tock    <- proc.time()[3]
 time[1] <- tock-tick

 tick    <- proc.time()[3]
 mat2    <- mat
 for(rep in 1:pow){
    mult2(mat2,k)
 }
 tock    <- proc.time()[3]
 time[2] <- tock-tick


# Results

 print(mat1[1,1:4])
 print(mat2[1,1:4])

 print(time)






