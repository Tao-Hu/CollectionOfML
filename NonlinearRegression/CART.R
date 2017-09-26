library(rpart)

# Generate a fake dataset

 set.seed(0820)

 n           <- 100
 p           <- 10
 x           <- matrix(rnorm(n*p),n,p)
 colnames(x) <- paste("cov",1:p)

 y <- 10+
      ifelse(x[,5]>0 & x[,2]>1,3,0)+ 
      ifelse(x[,5]<0,1,0)+
      rnorm(n)/10

# Split data into training and testing

   test  <- runif(n)<0.5
   yo    <- y[!test]
   xo    <- x[!test,]
   yp    <- y[test]
   xp    <- x[test,]

   train <- data.frame(x=xo,y=yo)
   test  <- data.frame(x=xp,y=yp)

# Fit a single tree

 fit1 <- rpart(y~.,train)
 plot(fit1)
 text(fit1, all=TRUE, cex=.8)

 yhat1 <- predict(fit1,newdata=test)
 mse1  <- mean((yp-yhat1)^2)

 plot(yp,yhat1,main=paste("MSE =",round(mse1,2)))

# Random forests

 library(randomForest)
 fit2 <- randomForest(y~.,data=train)

 print(fit2)
 importance(fit2) # importance of each predictor

 yhat2 <- predict(fit2,newdata=test)
 mse2  <- mean((yp-yhat2)^2)

 plot(yp,yhat2,main=paste("MSE =",round(mse2,2)))


 



