
# Comparison of classification methods for the homes data

##  Load the data
### The response is whether the sample is from the west coast

 load("S:\\Documents\\www\\BigData\\code\\homes.RData")

 s  <- homes[,5:4]
 Y  <- ifelse(homes[,3]=="CA" | homes[,3]=="OR" | homes[,3]=="WA",1,0)
 n  <- length(Y)

 library(maps)
 map("state")
 points(s,col=ifelse(Y==1,2,1))
 title("Class labels")

 X   <- as.matrix(OTU)
 X   <- ifelse(X>0,1,0)
 vvv <- apply(X,2,var)
 X   <- X[,vvv>0]          # Remove OTUs that are all 0 or all 1



## Split the data into training and testing


 set.seed(0820)
 test   <- rank(runif(n))<= 0.2*n
 s1    <- s[!test,]
 s2    <- s[test,]
 
 Xo     <- X[!test,]
 Yo     <- Y[!test]
 Xp     <- X[test,]
 Yp     <- Y[test]



## K nearest neighbors


 library(fields)
 dp      <- rdist(Xp,Xo)
 rank    <- apply(dp,1,rank)
 m       <- rowSums(rank<=5)
 p_knn10 <- Yp
 p_knn20 <- Yp
 p_knn30 <- Yp
 for(i in 1:length(Yp)){
    rrr        <- rank[i,]
    rrr        <- ifelse(rrr==min(rrr),0,rrr) # Deal with ties
    p_knn10[i] <- mean(Yo[rrr<=10]) 
    p_knn20[i] <- mean(Yo[rrr<=20]) 
    p_knn30[i] <- mean(Yo[rrr<=30]) 
 }

 boxplot(p_knn10~Yp,xlab="Y_p",ylab="p_hat",main="KNN with K=10")
 boxplot(p_knn20~Yp,xlab="Y_p",ylab="p_hat",main="KNN with K=20")
 boxplot(p_knn30~Yp,xlab="Y_p",ylab="p_hat",main="KNN with K=30")




## Sure independence screening


 r      <- abs(cor(Yo,Xo))
 keep   <- which(rank(-r) <= 200)
 Xsubo  <- Xo[,keep]
 Xsubp  <- Xp[,keep]


## Fit the LASSO


 library(glmnet)
 alpha  <- 1.0
 cv     <- cv.glmnet(Xsubo,Yo,family="binomial",alpha=alpha)
 plot(cv$lambda,cv$cvm)

 lambda <- cv$lambda.min
 lambda

 lasso   <- glmnet(Xsubo, Yo, family="binomial",alpha=alpha)
 best    <- which.min(abs(lasso$lambda-lambda))
 p_lasso <- predict(lasso,newx=Xsubp,type="response")[,best]

 boxplot(p_lasso~Yp,xlab="Y_p",ylab="p_hat",main="LASSO")



## Fit the elastic net



 library(glmnet)
 alpha  <- 0.5
 cv     <- cv.glmnet(Xsubo,Yo,family="binomial",alpha=alpha)
 plot(cv$lambda,cv$cvm)

 lambda <- cv$lambda.min
 lambda

 lasso   <- glmnet(Xsubo, Yo, family="binomial",alpha=alpha)
 best    <- which.min(abs(lasso$lambda-lambda))
 p_EN    <- predict(lasso,newx=Xsubp,type="response")[,best]

 boxplot(p_EN~Yp,xlab="Y_p",ylab="p_hat",main="EN")


## Fit ridge regression

 library(glmnet)
 alpha  <- 0.0
 cv     <- cv.glmnet(Xsubo,Yo,family="binomial",alpha=alpha)
 plot(cv$lambda,cv$cvm)

 lambda <- cv$lambda.min
 lambda

 lasso   <- glmnet(Xsubo, Yo, family="binomial",alpha=alpha)
 best    <- which.min(abs(lasso$lambda-lambda))
 p_ridge <- predict(lasso,newx=Xsubp,type="response")[,best]

 boxplot(p_ridge~Yp,xlab="Y_p",ylab="p_hat",main="Ridge")


## Discriminant analysis


 p0      <- colMeans(Xsubo[Yo==0,])
 p1      <- colMeans(Xsubo[Yo==1,])
 plot(p0,p1)

 p_DA <- Yp
 for(i in 1:length(Yp)){
    l0 <- prod(dbinom(Xsubp[i,],1,p0))
    l1 <- prod(dbinom(Xsubp[i,],1,p1))
    p_DA[i] <- l1/(l0+l1)
 }

 boxplot(p_DA~Yp,xlab="Y_p",ylab="p_hat",main="DA")


## Compare methods using Brier scores


 names <- c("KNN-10","KNN-20","KNN-30","LASSO","EN","RIDGE","DA")
 BS    <- NULL
 BS[1] <- mean((Yp-p_knn10)^2) # NN with K=10
 BS[2] <- mean((Yp-p_knn20)^2) # NN with K=20
 BS[3] <- mean((Yp-p_knn30)^2) # NN with K=30
 BS[4] <- mean((Yp-p_lasso)^2) # glmnet - LASSO
 BS[5] <- mean((Yp-p_EN)^2)    # glmnet - Elastic net
 BS[6] <- mean((Yp-p_ridge)^2) # glmnet - ridge
 BS[7] <- mean((Yp-p_DA)^2)    # DA

 names(BS)<-names
 round(BS,3)

## Compare methods using classification accuracy

 Yp1 <- ifelse(p_knn10>0.5,1,0)
 Yp2 <- ifelse(p_knn20>0.5,1,0)
 Yp3 <- ifelse(p_knn30>0.5,1,0)
 Yp4 <- ifelse(p_lasso>0.5,1,0)
 Yp5 <- ifelse(p_EN>0.5,1,0)
 Yp6 <- ifelse(p_ridge>0.5,1,0)
 Yp7 <- ifelse(p_DA>0.5,1,0)
 CA  <- NULL
 CA[1] <- mean(Yp==Yp1) # NN with K=10
 CA[2] <- mean(Yp==Yp2) # NN with K=20
 CA[3] <- mean(Yp==Yp3) # NN with K=30
 CA[4] <- mean(Yp==Yp4) # glmnet - LASSO
 CA[5] <- mean(Yp==Yp5) # glmnet - EN
 CA[6] <- mean(Yp==Yp6) # glmnet - Ridge
 CA[7] <- mean(Yp==Yp7) # DA

 names(CA)<-names
 round(CA,3)

 #Confusion table for ridge
 table(Yp,Yp6)


## Compare methods using classification accuracy

 library(pROC)
 roc1 <- roc(Yp,p_knn10)
 roc2 <- roc(Yp,p_knn20)
 roc3 <- roc(Yp,p_knn30)
 roc4 <- roc(Yp,p_lasso)
 roc5 <- roc(Yp,p_EN)
 roc6 <- roc(Yp,p_ridge)
 roc7 <- roc(Yp,p_DA)


 plot(roc1)
 lines(roc2,col=2)
 lines(roc3,col=3)
 lines(roc4,col=4)
 lines(roc5,col=5)
 lines(roc6,col=6)
 lines(roc7,col=7)
 
 n1<-paste("NN K=10 (AUC = ",round(roc1$auc,3),")",sep="")
 n2<-paste("NN K=20 (AUC = ",round(roc2$auc,3),")",sep="")
 n3<-paste("NN K=30 (AUC = ",round(roc3$auc,3),")",sep="")
 n4<-paste("LASSO (AUC = ",round(roc4$auc,3),")",sep="")
 n5<-paste("EN (AUC = ",round(roc5$auc,3),")",sep="")
 n6<-paste("Ridge (AUC = ",round(roc6$auc,3),")",sep="")
 n7<-paste("DA (AUC = ",round(roc7$auc,3),")",sep="")

legend("bottomright",c(n1,n2,n3,n4,n5,n6,n7),lty=1,col=1:7,inset=0.05)


## Maps of test set prediction


  map("state")
  title("Test set classifications - NN K=10")
  points(s2,pch=19,col=ifelse(p_knn10>0.5,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)

  map("state")
  title("Test set classifications - NN K=20")
  points(s2,pch=19,col=ifelse(p_knn20>0.5,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)

  map("state")
  title("Test set classifications - NN K=30")
  points(s2,pch=19,col=ifelse(p_knn30>0.5,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)

  map("state")
  title("Test set classifications - LASSO")
  points(s2,pch=19,col=ifelse(p_lasso>0.5,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)

  map("state")
  title("Test set classifications - Elastic net")
  points(s2,pch=19,col=ifelse(p_EN>0.5,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)

  map("state")
  title("Test set classifications - Ridge")
  points(s2,pch=19,col=ifelse(p_ridge>0.5,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)

  map("state")
  title("Test set classifications - DA")
  points(s2,pch=19,col=ifelse(p_DA>0.5,2,1))
  legend("bottomright",c("West coast","Not"),col=2:1,pch=19)

