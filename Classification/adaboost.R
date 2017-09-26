
# Adaboost to predict whether a dust sample is from the west coast


## Load the data

 load("S:\\Documents\\www\\BigData\\code\\homes.RData")
 
 s  <- homes[,5:4]
 Y  <- ifelse(homes[,3]=="CA" | homes[,3]=="OR" | homes[,3]=="WA",1,-1)
 n  <- length(Y)

 library(maps)
 map("state")
 title("Class labels")
 points(s,col=ifelse(Y==1,2,1))
 

 table(Y)
 mean(Y==1)
 mean(Y== -1)

 X  <- as.matrix(OTU)
 X  <- ifelse(X>0,1,0)
 v  <- apply(X,2,var)
 X  <- X[,v>0]


## Sure independence screening
 
  r       <- abs(cor(Y,X))
  keepers <- rank(-r)<=100
  X       <- X[,keepers]
  dim(X)

## Split into training and testing
 
   set.seed(0820)
   test  <- runif(n)<0.2

   n1    <- sum(!test)
   n2    <- sum(test)

   s1    <- s[!test,]
   s2    <- s[test,]
   train <- data.frame(x=X[!test,],y=Y[!test])
   test  <- data.frame(x=X[test,],y=Y[test])

## Perform adaboost step-by-step
 
        
  library(rpart)

 # Number of iterations
 
  M       <- 50

  Gm      <- matrix(0,n1,M)  
  Gm_test <- matrix(0,n2,M)  
  alpha   <- rep(0,M)
  error   <- rep(0,M)

  # Initial weights
  w       <- rep(1/n1,n1)

  for(m in 1:M){

   # Weighted regression tree

    fit         <- rpart(as.factor(y)~.,train,weights=w)   

   # Compute the classifier on training data

    prob1       <- predict(fit,newdata=train)[,2]
    Gm[,m]      <- ifelse(prob1>0.5,1,-1)

   # Compute the classifier on testing data
 
    prob1       <- predict(fit,newdata=test)[,2]
    Gm_test[,m] <- ifelse(prob1>0.5,1,-1)
 
   # Update the weights

    miss        <- train$y!=Gm[,m]
    error[m]    <- sum(w*miss)/sum(w)
    alpha[m]    <- log((1-error[m])/error[m])
    w           <- w*exp(alpha[m]*miss)

   # Plot this iteration's tree

    if(m<5 | m==M){
     e <- round(error[m],2)
     plot(fit,main=paste("m =",m," and error =",e))
     text(fit, all=TRUE, cex=.8)  
    }
  }

  plot(error)
  plot(alpha)
  plot(w)
  
  map("state",main="Final adaboost weights")
  points(s1,cex=50*w/sum(w),pch=19,col=ifelse(rank(w)>0.9*n1,2,1))


## Compute the final classifier

  G     <- sign(Gm%*%alpha) 
  table(G,train$y)

  Gtest <- sign(Gm_test%*%alpha) 
  table(Gtest,test$y)

  map("state")
  title("Test set classifications")
  points(s2,pch=19,col=ifelse(Gtest==1,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)




## Plot classification accuracy by M

  acc    <- rep(0,M)
  Yhat   <- Gm_test[,1]
  acc[1] <- mean(test$y==Yhat)

  for(m in 2:M){ 
    Yhat <- sign(Gm_test[,1:m]%*%alpha[1:m]) 
    acc[m] <- mean(test$y==Yhat)
  }

  plot(acc,xlab="M",ylab="Classification accuracy",ylim=c(0.8,1))
  abline(mean(Y== -1),0,col=2)



## Adaboost using the R package ada

  library(ada)
  fit <- ada(y~., train)
  plot(fit)
  summary(fit)
  Yhat <- predict(fit,test)
  table(Yhat,test$y)
  mean(Yhat==test$y)
  mean(Y== -1)

  map("state")
  title("Test set classifications")
  points(s2,pch=19,col=ifelse(Yhat==1,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)

