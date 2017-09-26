library(inline)
library(Rcpp)

# Rcpp function to multiply a matrix by a constant

 scalemult_code = "

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

 scalemultmatrix <- cxxfunction(signature(x = "numeric",k = "numeric"), 
                                body=scalemult_code, 
                                plugin="Rcpp")


# Rcpp function to raise a matrix to a power

 power_code = "

  Rcpp::NumericMatrix xcpp(x);
  int nr = xcpp.nrow();
  int nc = xcpp.ncol();
  double kcpp = Rcpp::as<double>(k);
  int powcpp = Rcpp::as<int>(pow);

  Rcpp::NumericMatrix xnew(nr, nc);

  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      xnew(i,j) = xcpp(i,j);
    }
  }


  for(int r = 0; r<powcpp; r++){
   for (int i = 0; i < nr; i++){
    for (int j = 0; j < nc; j++){
        xnew[nr * j + i] *= kcpp;
    }
   }
  }

  return xnew;

  "

 powermatrix <- cxxfunction(signature(x = "numeric",k = "numeric",pow="integer"),
                            body=power_code,
                            plugin="Rcpp")


# Timing experiment

 n     <- 10^2
 pow   <- 10^6
 k     <- k <- exp(log(3)/pow)
 mat   <- diag(n)+1

 # the work below creates the matrix (k^pow)*mat four ways
  
 time        <- rep(0,4)
 names(time) <- c("R loop","R mat","C + R","C")

# (1) Double the matrix in R with a loop

if(FALSE){
 tick    <- proc.time()[3]
 mat1    <- mat
 for(rep in 1:pow){for(i in 1:n){for(j in 1:n){
   mat1[i,j] <- k * mat1[i,j]
 }}}
 tock    <- proc.time()[3]
 time[1] <- tock-tick
}

# (2) Double the matrix in R without a loop

 tick    <- proc.time()[3]
 mat2    <- mat
 for(rep in 1:pow){
  mat2    <- k*mat2
 }
 tock    <- proc.time()[3]
 time[2] <- tock-tick

# (3) Call Rcpp inside the loop

 tick    <- proc.time()[3]
 mat3    <- mat
 for(rep in 1:pow){
   mat3 <- scalemultmatrix(mat3,k)
 }
 tock    <- proc.time()[3]
 time[3] <- tock-tick

# (4) All computions in Rcpp

 tick    <- proc.time()[3]
 mat4    <- powermatrix(mat,k,pow)
 tock    <- proc.time()[3]
 time[4] <- tock-tick

# Results

 print(mat1[1,1:4])
 print(mat2[1,1:4])
 print(mat3[1,1:4])
 print(mat4[1,1:4])

 print(time)
 print(time/time[2])






