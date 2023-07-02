#HW1

library('igraph')
library('dequer')
tr<- graph(edges=c(1,2,1,3,1,4,1,5,2,3,3,4,4,5,5,2,4,6,5,6), n=6, directed = F)
tr2 <- graph(edges=c(1,2,1,3,1,4,1,5,2,3,3,4,4,5,5,2), n=5, directed = F)


#paths have no repeated edges, so we omit the i=j
paths_l2 <- function(g){
  adj <- as_adjacency_matrix(g)
  adj_2<- adj%*%adj
  s=0
  for(i in 1:nrow(adj_2)){
    for(j in 1:ncol(adj_2)){
      if (i!=j){
        s <- s +adj_2[i,j]
      }
    }
  }
  a<- (s/2)
  return(a)
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

transitivity_g <- function(g){
  c3<- total_triangles(g)
  p2 <- paths_l2(g)
  t_g<- (3*c3)/p2
  return(t_g)
}






######  HW2
#this is what I get that we need to do with the function
#1 if only a graph is given as input: the number of shortest path of all nodes to any other node (depending on the gr. there might be more than one sh.path.)
#2 if a graph, u and v nodes and k-length are given:  number of k-length walks between u and v
#I have copy-pasted the codes for functions from previous hw which I call within the the last funnction "multiple_things()".
  
queue_empty <-function(q){
  if (length(q)==0)
    s<-TRUE
  else
    s<- FALSE
  return(s)
}

#bfs algorithm modified for the number of shortest paths according to lecture3/slide47,48
bfs_shortest_path<- function(g,v){
  
  q <- queue() 
  num_nodes <- length(V(g))
  mark <- rep(0, num_nodes) 
  mark[v]<-1
  dist <- rep(Inf, num_nodes)
  dist[v] <-0 
  path <- rep(0, num_nodes) 
  path[v] <- 1
  pushback(q,v)
  num <- rep(0, num_nodes) 
  num[v] <- length(which(mark==1))
  #while the queue of the nodes we still need to process is not empty
  while(queue_empty(q)==F){
    u <- pop(q) #remove the item on the top of the queue 
    adj<- which(g[u,]==1)
    for(w in adj){
        if (dist[w] > dist[u]+1){
          dist[w]<- dist[u]+1
          path[w]<- path[u]
        }
        else if (dist[w] == dist[u]+1) {
          path[w]<- path[u]+path[w]
        }
      if (mark[w]!=1){
        mark[w]<-1
        pushback(q,w)
      }
      }
    }
  return(path) 
}






all_nodes_shortest_paths<- function(g){
  num.nodes<- length(V(g))
  markk<- rep(0, num.nodes)
  f_res <- NULL
  for(vert in V(g)){
    if(markk[vert]!=1){
      f_res<- cbind(f_res, bfs_shortest_path(g,vert))
    }
  }
  return(f_res) #for some reason it gave an extra col of 0s
}



MatrixPower<- function(mat, k){
  res<- NULL
  if(k==1){ res<- mat}
  else{
    R <- MatrixPower(mat, floor(k/2))
    if(k%%2==0){res<- R%*%R}
    else{ res<-mat%*%R%*%R }
  }
  return(res)
}


paths_kl <- function(g, k){
  adj <- as_adjacency_matrix(g)
  adj_k<- MatrixPower(adj, k)
  return(adj_k)
}

multiple_things <- function(g, u=0, v=0, k){
  if(u!=0 && v!=0){
    paths_k <- paths_kl(g,k)
    paths_k_uv<- paths_k[u,v]
    short_p <- all_nodes_shortest_paths(g)
    return(list(paths_k_uv, short_p[u,v]))
  }
  else 
  {
    short_p <- all_nodes_shortest_paths(g)
    return(short_p)
  }
}
