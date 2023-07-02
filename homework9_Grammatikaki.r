library('igraph')

##ex1

#the function will produce the same results as the build-in modularity function, if cluster_walktrap instead of laplacian is used
#it returns the modularity and the formed clusters
#it takes as input the graph and 'membership vectors', if no is provided, it calculates them with the Laplacian function

MyModularity<- function(g, w=Laplacian(g)) {
  #w<- Laplacian(g)
  
  for (i in 1:length(w)){
    w[[i]]<- as.integer(w[[i]])
  }
  total_edges<- length(E(g))
  b<- NULL
  for (i in 1:length(w)){
    a<- (length(E(induced.subgraph(g,w[[i]])))/total_edges)
    b<- append(b,a)
  }
  t<-NULL
  d<- degree(g)
  for (i in 1:length(w)){
    s<- (sum(d[w[[i]]]))
    t<- append(t,s)
  }
  t<- (t/(2*total_edges))
  t<-(t)^2
  res<- sum(b)-sum(t)
  return(list(res, w))
}

  
#to test
mt=matrix(c(0,1,1,1,0,0,0,0,1,0,1,0,1,1,0,0
              ,1,1,0,0,0,1,0,0,1,0,0,0,0,0,1,1
              ,0,1,0,0,0,1,0,0,0,1,1,0,1,0,0,0
              ,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0),nrow=8,ncol=8)
  
  
g=graph_from_adjacency_matrix(mt,mode="undirected")

MyModularity(g)  




##ex2 
#according to slide   56/lecture7
#I am not sure if it is actually the alg. on sl.55 or 56 that we had to implement   
# 

g1<- make_graph(edges=c(1,2,2,3,2,4,3,4,4,5,5,6), n=6, directed=FALSE)

ImproveModularity<- function(g){
 a<- MyModularity(g)
 Mod1<- a[[1]]
 Clusters1 <- a[[2]]
 moved<- c()
 mod_increase<- c()
 while (length(moved)<=length(V(g)) | mod_increase!=0 ){
   for (i in 1:length(V(g)))
     moved<- append(moved,i)
     b<- which(sapply(w, FUN=function(X) "i" %in% X))
 }
   
 
}





### previous functions 

Laplacian <-function(graph){
    
    if(is.null(V(graph)$name))
      V(graph)$name <-as.character(1:length(V(graph)))
    
    clusters <-list()
    
    count<-1
    
    while(length(V(graph))>4)
    {
      A <-as_adj(graph,type ="both",sparse =F)
      D <-diag(degree(graph))
      L <-D-A 
      eig <-eigen(L)
      id <-order(eig$values,decreasing =F)[2]
      
      tmp<-list()
      
      tmp[[1]]<-which(eig$vectors[,id]<0)
      tmp[[2]]<-which(eig$vectors[,id]>0)
      tmp[[3]]<-which(eig$vectors[,id]==0)
      
      len <-unlist(lapply(tmp,length))
      len[len==0]<-100000
      clusters[[count]]<-V(graph)$name[tmp[[which.min(len)]]]
      
      graph <-induced.subgraph(graph =graph,vids =as.character(c(setdiff(V(graph)$name,clusters[[count]]))))
      
      count <-count+1
      
    }
    
    if(length(V(graph))<=4)
    {
      comp_g <-components(graph,mode ="strong")
      for(i in 1:comp_g$no)
      {
        clusters[[count]]<-V(graph)$name[which(comp_g$membership==i)]
        count <-count+1
      }
    }
    return(clusters)
}
  
