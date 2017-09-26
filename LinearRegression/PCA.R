
# Principal components analysis (PCA) for the homes data


## Extract the variables


 load("S:\\Documents\\www\\BigData\\code\\homes.RData")
 lat      <- homes[,4]
 long     <- homes[,5]
 temp     <- homes[,6]
 precip   <- homes[,7]
 NPP      <- homes[,8]
 elev     <- homes[,9]
 house    <- ifelse(homes[,10]=="One-family house detached from any other house",1,0)
 bedrooms <- as.numeric(homes[,11])

 X        <- cbind(lat,long,temp,precip,NPP,elev,house,bedrooms)

 miss     <- is.na(rowSums(X))
 X        <- X[!miss,]
 pairs(X)
 
 kable(round(cor(X),2))


## PCA

 X <- scale(X) # now each variable has mean zero and varaince 1
 e <- eigen(cov(X))

 eval <- e$values
 pct  <- cumsum(eval)/sum(eval)

 plot(eval,ylab="Eigenvalue")
 plot(pct,ylab="Proportion of variance explained")

 evec1<-e$vector[,1]
 evec2<-e$vector[,2]
 evec3<-e$vector[,3]

 names(evec1)<-names(evec2)<-names(evec3)<- colnames(X)

 #evec1: measures (long+temp+precip+NPP) - (lat+elev) 
 round(evec1,2)

 #evec2: measures lat-temp
 round(evec2,2)

 #evec3: captures correlation between house and bedrooms
 round(evec3,2)


## Contruct new covariates as linear combinations of $X$

 Z <- X%*%e$vectors

 X[1,]
 Z[1,1:3]

 #House 1, constructed covariate 1
  sum(X[1,]*evec1)

 #House 1, constructed covariate 2
  sum(X[1,]*evec2)

 #House 1, constructed covariate 3
  sum(X[1,]*evec3)
   

## Inspect the constructed covariates

  plot(X[,1],Z[,2],xlab="Lat",ylab="Z2")
  plot(X[,3],Z[,2],xlab="Temp",ylab="Z2")
  
  plot(X[,2],X[,1],col=ifelse(Z[,2]>0,2,4),pch=19,
       xlab="Long",ylab="Lat",main="Z2>0 is red")


  boxplot(Z[,3]~X[,7],xlab="House",ylab="Z3")
  boxplot(Z[,3]~X[,8],xlab="Bedrooms",ylab="Z3")




