library('igraph')
#k=3
g1<- make_graph(edges=c(1,2,2,3), n=3, directed = FALSE)
g2<- make_graph(edges=c(1,2,2,3,1,3), n=3, directed = FALSE)

## k=4
g3<- make_graph(edges=c(1,2,2,3,3,4), n=4, directed = FALSE)
g4<- make_graph(edges=c(1,2,2,3,2,4), n=4, directed = FALSE)
g5<- make_graph(edges=c(1,2,2,3,3,4,4,1), n=4, directed = FALSE)
g6<- make_graph(edges=c(1,2,2,3,2,4,3,4), n=4, directed = FALSE)
g7<- make_graph(edges=c(1,2,1,3,3,4,2,4,1,4), n=4, directed = FALSE)
g8<- make_graph(edges=c(1,2,1,3,2,3,4,1,4,2,4,3), n=4, directed = FALSE)


#graph for testing the functions
gr<- make_graph(edges=c(1,2,2,5, 5,6, 6,7,6,8,1,3,2,3,2,4,3,4,3,9,3,10, 4,9,4,10, 9, 10), n=10, directed = FALSE)

########################################################################################
#the graphlet descriptor can be used as a way for finding motifs within a graph that are repetitive 
#the complexity of the algorithm is polynomial and is increasing as k increases. For k=3 O(n^3) and for k=4 O(n^4)


FindAll3kGraphlets<- function(g){
  res<- matrix(ncol=length(V(g)), nrow=1)
  for (i in 1:length(V(g))){
    a<- Find3kGraphlets(g,i)
    res[1,i]<-a
  }
  s<- sum(res)/2
  b<- total_triangles(g)
  return(list(s,b))
}



Find3kGraphlets<- function(g,n){
  Nb1<- NodeNeighbor1(g,n)
  Nb2<- c()
  counter<-0
  for(i in Nb1){
    a<- NodeNeighbor1(g, i)
    a<- a[a!=n]
    if (length(a)==0){
      Nb1<- Nb1[Nb1!=i]}
    if (length(a)!=0){
      counter<- counter+length(a)
    }
    Nb2<- append(Nb2,a)
    Nb2<- unique(Nb2)
    Nb2<- setdiff(Nb2,Nb1)
  }
  return(counter)
}





FindAll4kGraphlets<- function(g){
  res<- NULL
  for (i in 1:length(V(g))){
    a<- Find4kGraphlets(g,i)
    res <- rbind(res, a)
  }
  c<- colSums(res)
  c[1]<-c[1]/2
  c[2]<-c[2]/3
  c[3]<- c[3]/4
  c[4]<- c[4]/3
  c[5]<- c[5]/2
  c[6]<- c[6]/4
  return(c)
}


Find4kGraphlets<- function(g,n){
  Nb1<- NodeNeighbor1(g,n)
  Nb2<- c()
  Nb3<- c()
  #counters per graphlet
  g3<-0
  g4<-0
  g5<-0
  g6<-0
  g7<-0
  g8<-0
  for (i in Nb1){
    a<- NodeNeighbor1(g,i)
    a<- a[a!=n]
    Nb2<- append(Nb2, a)
    Nb2<- unique(Nb2)
  }
  for(j in Nb2){
    b<- NodeNeighbor1(g,j)
    b<- b[b!=n]
    b<- setdiff(b,Nb1)
    Nb3<- append(Nb3, b)
    Nb3<- unique(Nb3)
  }
  #return(list(n, Nb1, Nb2, Nb3))
  for (i in Nb1){
    for (j in Nb2){
      for (k in Nb3){
        nodes<- c(n,i,j,k)
        H<- induced.subgraph(g,nodes)
        if (length(E(H))==6){
          g8<-g8+1
        }
        if (length(E(H))==5){
          g7<-g7+1
        }
        if (length(E(H))==4){
          d<- degree(H)
          if (any(d==3)){
            g6<- g6+1
          }else{
            g5<-g5+1
          }
        }
        if (length(E(H))==3){
          d<- degree(H)
          if (any(d==3)){
            g4<-g4+1
          }
          else{
            g3<-g3+1
          }
        }
      }
    }
  }
  g7<-g7/2
  g6<-g6/2
  g5<-g5/2
  x<-cbind(g3, g4,g5, g6, g7, g8)
  return(x)
}




FindAll3kGraphlets(gr)
FindAll4kGraphlets(gr)




#################    from previous homework   ###########################





NodeNeighbor1 <- function(g, node){
  mat<- as_adj(g)
  Nb1 <-vector()
  chop1 <- array(mat[node,])
  for(i in 1:dim(chop1)){
    if (chop1[i]==1){
      Nb1<- append(Nb1,i)
    }
  }
  return(Nb1)
}

total_triangles <- function(g){
  adj <- as_adjacency_matrix(g)
  adj_3<- adj%*%adj%*%adj
  s = 0
  for(i in 1:nrow(adj_3)){
    for (j in 1:ncol(adj_3)){
      if (i==j){
        s <- s +adj_3[i,j]
      }
    }
  }
  triangles <- s/6
  return(triangles)
}

