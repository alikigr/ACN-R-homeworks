library('dequer')
library('igraph')
g1 <- graph( edges=c(1,2, 2,3, 2,4, 1,5, 5,6), n=6, directed=F )

##### HW1
queue_empty <-function(q){
  if (length(q)==0)
    s<-TRUE
  else
    s<- FALSE
  return(s)
}

  
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
  #while the queue of the nodes we still need to process is not empty
  while(queue_empty(q)==F){
        u <- pop(q) #remove the item on the top of the queue 
        for(w in (1:length(V(g)))){
          if ((g[u,w] != 0) && (mark[w] != 1)){
            mark[w] <-1
            pushback(q,w)
            if (dist[w] > dist[u]+1){
              dist[w]<- dist[u]+1
              path[w]<- path[u]
            }
            else if (dist[w] == dist[u]+1) {
              path[w]<- path[u]+path[w]
            }
          }
        }
      }
    return(dist) 
}


all_nodes_shortest_paths<- function(g){
  num.nodes<- length(V(g))
  f_res <- NULL
  for(ver in V(g)){
      f_res<- cbind(f_res, bfs_shortest_path(g,ver))
    }
  return(f_res) 
}




#### HW2
## does not make sense 
find_eccentricity <- function(g){
  p<- all_nodes_shortest_paths(g)
  ecc<- NULL
  for (i in 1:nrow(p)){
    ecc[i]<- which(mt[i,]==max(mt[i,]))[1]
  }
  return(ecc)
}

visualize_center<- function(g){
  center_nodes<- find_eccentricity(g)
  V(g)$color <- "black"
  V(g)[center_nodes] <- "red"
  plot(g)
}
