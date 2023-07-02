library('igraph')

### HW2
CloseCentr <- function(g,u){
  x <- paths(g,u)
  len <- x[[1]]
  sum_len <- sum(len)
  centr <- 1/sum_len
  return(centr)
} 


### 2a
erd<- erdos.renyi.game(n=10, p.or.m = T)
bar <- barabasi.game(n=10)


EveryCloseCentr <- function(g){
  v<- c()
  l<- length(V(g))
  for( i in 1:l){
    v<- append(v, CloseCentr(g,i))
  }
  return(v)
}

a<- EveryCloseCentr(erd)
b<- EveryCloseCentr(bar)

hist(a)
hist(b)

#the values of the closeness centrality for an Erdos-Renyi graph are equal for each node since in such a graph
#every node is connected with every other node. On the other hand, bar is a scale free network and the degree of each node varies. 
#Consequently the centrality of each of them also varies. 

### 2b
CloseCentr_Normalized <- function(g,u){
  x <- paths(g,u)
  len <- x[[1]]
  N<- length(V(g))
  sum_len <- sum(len)
  centr <- (N-1)/sum_len
  return(centr)
} 

EveryCloseCentr_Normalized <- function(g){
  v<- c()
  l<- length(V(g))
  for( i in 1:l){
    v<- append(v, CloseCentr2(g,i))
  }
  return(v)
}


centr_clo()
#the build-in function for small graphs works with the formula
# (N-1)/(sum of length of shortest path from one node to all the other nodes)
#therefore the values of this function are bigger than the one without the N-1.




### HW3
g<- graph(edges = c(1,2,2,4,4,3,3,4,3,2,3,1,1,3), n=4)



Make_P_Mat <- function(g){
  A<- as.matrix(g[])
  S<- NULL
  for(i in 1:nrow(A)){
    S<- rbind(S,sum(A[i,]))
  }
  for(i in 1:nrow(A)){
    for(j in 1:ncol(A)){
      if (A[i,j]==1){
        n<-A[i,j]/S[i]
        A[i,j]=n
      }
    }
  }
  return(t(A))
}


## call it with a big enough k, so you get to convergence
PageRank<- function(g,a,k){
  L<- Make_P_Mat(g)
  n<- length(V(g))
  r <- matrix(rep(1/n, n))
  ones<- matrix(rep(1,n))
  res<-NULL
  for (i in 1:k){
    Q<- MatrixPower(L,i)
    R<- a*Q%*%r + ((1-a)/n)*ones
    res<- cbind(res, R)
  }
  return(res)
}


### HW1
g0<- graph(edges=c(1,2,1,3,2,3,2,5,3,6,5,6,1,4,4,7,4,8,7,8),n=8, directed = F)
g1 <- graph( edges=c(1,2, 2,3, 2,4, 1,5, 5,6), n=6, directed=F )

##hw1

FindEcc <-function(g){
  D <-floyd(g)[[1]]
  ecc <-c()
  for (i in 1:nrow(D)){
    ecc[i]<-max(D[i,])
  }
  return(ecc)
}


FindAllPathsThrough<- function(g){
  m<- as.matrix(g[])
  D<- floyd(g)[[1]]
  P<-num_paths(g)
  sol<- array(0, dim=ncol(m))
  for(w in 1:nrow(m)){
    for(u in 1:nrow(m)){
      for(v in 1:nrow(m)){
        q<-0
        if (v>=u){
          next
        } else if(w!=u && w!=v){
          q<- num_paths_through(g,u,v,w)
        }
        sol[w]<- sol[w]+q
      }
    }
  }
  return(sol)
}


NodeCorrel <- function(g){
  x<- FindEcc(g)
  y<- FindAllPathsThrough(g)
  return(cor(x,y))
}

erd<- erdos.renyi.game(n=10, p.or.m = T)
bar <- barabasi.game(n=10)



#############    FUNCTIONS MADE IN PREVIOUS HW OR DURING THE EXERCISE SESSIONS AND ARE USED WITHIN THIS HW
#FOR HW1


floyd<-function(g){
  D <-as.matrix(g[])
  D[which(D == 0)]<-100000
  P <-matrix(0, nrow(D), ncol(D))
  for(i in 1:(nrow(D)-1)){
    for (j in (i+1):nrow(D)){
      if (D[i,j]== 1){
        P[i, j]<-j
      }
      if(D[j, i]== 1){
        P[j, i]<-i
      }
    }
  }
  for(i in 1:nrow(D)){
    D[i,i]<-0
    P[i,i]<-i
  }
  for(k in 1:nrow(D)){
    for (i in 1:nrow(D)){
      for (j in 1:nrow(D)){
        if (D[i,j]> D[i,k]+ D[k, j]){
          D[i,j]<-D[i,k]+ D[k, j]
          P[i,j]<-P[i,k]
        }
      }
    }
  }
  return(list(D,P))
}

#Input: floyd[[2]], nodes u,v. Output: path from u to v
shortest_p <-function(P, u, v){
  path<-c()
  selected_edges <-c()
  if (P[u, v]!= 0){
    path <-c(path, u)
    while (u != v){
      u <-P[u, v]
      path<-c(path, u)
    }
    for(i in 1:(length(path)-1)){
      selected_edges    <-c(selected_edges,path[i], path[i+1])
    }
  }
  return(selected_edges)
}


#BFS for distance of shortest path from a given node to all other and the num of such available paths from the node to the others 
paths<- function(g,u){
  visited<-c(u)
  count<-1
  dist <-rep(100000, length(V(g)))
  path <-rep(0, length(V(g)))
  dist[u]<-0
  path[u]<-1
  while(length(visited)!= length(V(g))){
    curr<-which(dist == (count -1))
    for (i in 1:length(curr)){
      nei_i<-setdiff(neighborhood(g, 1, curr[i])[[1]], visited)
      for (j in 1:length(nei_i)){
        if (dist[nei_i[j]]> (dist[curr[i]]+ 1)){
          dist[nei_i[j]]<-dist[curr[i]]+1
          path[nei_i[j]]<-path[curr[i]]
        }
        else if (dist[nei_i[j]]==(dist[curr[i]]+1)){
          path[nei_i[j]]<-path[curr[i]]+path[nei_i[j]]
        }
      }
    }
    count<- count+1
    visited<-union(visited, curr)
  }
  return(list(dist,path))
}

#number of shortest paths of each node to any other node
num_paths <-function(g){
  P <-matrix(0, length(V(g)), length(V(g)))
  for (i in 1:length(V(g))){
    P[i,]<-paths(g, i)[[2]]
  }
  return(P)
}

#number of shortest paths through w
#input: D from floyd's, P from num_paths
num_paths_through<-function(g, u, v, w){
  D<- floyd(g)[[1]]
  P<- num_paths(g)
  sol <-0
  if ((D[u,w]+D[w,v])==D[u, v]){
    sol <-P[u,w]*P[w,v]
  }
  return(sol)
}


#FOR HW3
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