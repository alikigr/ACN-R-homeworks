####  HW1
##degree of a vertex: the number of its adjacent edges
library('igraph')
g1 <- graph( edges=c(1,2, 2,3, 2,4, 1,5, 5,6), n=6, directed=F )


NodeDegree <- function(mat){
  d<- rep(0,nrow(mat))
  for(i in 1:nrow(mat)){
    d[i]<- sum(mat[i,])
  }
  return(d)
}


NodeDegree(g1[])
degree(g1)




####  HW2
FirstNeighbor <- function(mat, node){
  nb <- which(mat[node,]==1)
  return(nb)
} 




#### HW3
SecondNeighbor <- function(mat, node){
  nb1 <- which(mat[node,]==1)
  nb2<-c()
  for (k in nb1){
    nb2 <- c(nb2, which(mat[k,]==1))
  }
  nb2 <- unique(c(nb1, nb2))
  nb2 <- nb2[nb2!=node]
  return(nb2)
}




#### HW4  (not finished)
MostSharedNeighbors <- function(mat){
  Nb<- c()
  for (k in 1:nrow(mat)){
    chop<-array(mat[k,])
    for (i in 1:dim(chop)){
      if (chop[i]==1){
        Nb <- c(Nb,i)
      }
    }
  }
  return(Nb)
}

