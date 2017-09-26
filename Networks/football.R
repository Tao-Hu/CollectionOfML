library(network)

# Load the football data

 dat <- read.csv("http://www4.stat.ncsu.edu/~reich/BigData/code/winnerLoser_analysis.csv",
                 stringsAsFactors = FALSE)

# The winning and losing teams are in columns 1 and 2

 w   <- dat[,1]
 l   <- dat[,2]

# Covert team names to team ID numbers

 teams  <- sort(unique(c(w,l)))
 n      <- length(w)
 m      <- length(teams)

 ID     <- matrix(NA,n,2)
 record <- matrix(NA,m,2)

 for(i in 1:m){
  ID[,1] <- ifelse(w==teams[i],i,ID[,1])
  ID[,2] <- ifelse(l==teams[i],i,ID[,2])
  record[i,1] <- sum(w==teams[i])
  record[i,2] <- sum(l==teams[i])
 }

 colnames(record) <- c("W","L")
 rownames(record) <- teams

 record[1:5,]
 ID[1:5,]

 A           <- matrix(0,m,m)
 rownames(A) <- teams
 colnames(A) <- teams

 for(j in 1:n){
    A[ID[j,1],ID[j,2]]<-1
 }
 
 # Restrict to teams with at least 5 games

 ngames <- record[,1] + record[,2]
 A      <- A[ngames>5,]
 A      <- A[,ngames>5]
 pct    <- record[ngames>5,1]/(record[ngames>5,2] + record[ngames>5,1])


 # Directed graph (winner points to loser):

  g <- network(A)
  plot(g,label=rownames(A),vertex.col=ifelse(pct>0.5,2,3))

  legend("topright",c("Winning record","Losing record"),col=2:3,pch=19,cex=1.5)

 # Undirected graph (an edge means they played a game):

  C <- A+t(A)
  g <- network(C)
  plot(g,label=rownames(C),vertex.col=ifelse(pct>0.5,2,3),arrow=F)

  legend("topright",c("Winning record","Losing record"),col=2:3,pch=19,cex=1.5)

 # Clustering

  library(igraph)

  g <- graph_from_adjacency_matrix(A, mode = "undirected")
  plot(g)

  c <- fastgreedy.community(g)
  dendPlot(c,mode="hclust")

 # Spectral partitioning

  Laplace <- diag(rowSums(C))-C
  E       <- eigen(Laplace)
  plot(E$val)

  EV1 <- E$vec[,127]
  EV2 <- E$vec[,126]

  plot(EV1,EV2,pch=19,xlab="",ylab="")
  text(EV1,EV2,rownames(C),cex=.6,pos=3)


 # Spectral partitioning

  Laplace <- diag(rowSums(C))-C
  E       <- eigen(Laplace)
  plot(E$val)

  EV1 <- E$vec[,127]
  EV2 <- E$vec[,126]

  plot(EV1,EV2,pch=19,xlab="",ylab="")
  text(EV1,EV2,rownames(C),cex=.6,pos=3)



