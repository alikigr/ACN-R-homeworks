library('igraph')

#hw1
#I made a fuction in which takes a graph, makes its complementary and calculates the correlation of betweenness and pagerank centrality measures
#then it generates 200 more graphs of the same degree, calculates the same things, and it calculates the emp pvalue of receiving a positive correlation 
#between the centralities of the graph and its complementary. It returns the observed corr and the probability of encountering a positive corr. 
e<- make_tree(n=12, mode = 'undirected')


Complementary_Centrality<- function(g){
  c<- complementer(g)
  Cor_ObsB<- cor(betweenness(g), betweenness(c))
  Cor_ObsPR<- cor(page_rank(g)[[1]], page_rank(c)[[1]])
  deg<- degree(g)
  counter1<-0
  counter2<-0
  for (i in 1:200){ #B=200
    g_new<- configuration(deg)
    c_new<- complementer(g_new)
    Cor_PermB<- cor(betweenness(g_new), betweenness(c_new))
    Cor_PermPR<- cor(page_rank(g_new)[[1]], page_rank(c_new)[[1]])
    if (Cor_PermB>0){
      counter1<-counter1+1}
    if (Cor_PermPR>0){
      counter2<-counter2+1}
  }
  emp_pvalue1<- (counter1+1)/(201)
  emp_pvalue2<- (counter2+1)/(201)
  return(list(Cor_ObsB, emp_pvalue1,Cor_ObsPR, emp_pvalue2))
}

Complementary_Centrality(e)
#both centrality measures are negatively correlated 


#degree distribution: for a given degree k, the distribution of it is P(k)=nk/n
#nk is the number of nodes in the graph with degree k, and n the total number of nodes


#if we know the degree distribution of the graph, we also know the one of its complementary, as they are equal

DistributionDegree<- function(g){
  deg_g<- degree(g)
  deg_c<- degree(complementer(g))
  ung<- unique(deg_g)
  unc<- unique(deg_c)
  n<- length(V(g))
  distr<- NULL
  distr2<- NULL
  for(i in 1:length(ung)){
    nk<- length(which(deg_g==ung[i]))
    Pk<- nk/n
    distr<- append(distr, Pk)
  }
  for (i in 1:length(unc)){
    nk2<-length(which(deg_c==unc[i]))
    Pk2<- nk2/n
    distr2<- append(distr2, Pk2)
  }
  return(list(distr, distr2))
}


##hw 2
#I start from a node n, and I compute the 1st neighborhood of it. Then I compute the neighborhood of each element
#in the 1st neighborhood, if any of them does not have any other neighbor other than its parent-node, then it is removed from Nb1
#in the second function, I try to implement the same thing for the elements of the 2nd neighborhood. My for loop there should iterate
#through the elements of Nb2-Nb1, however at least with my example this does not work as expected. 

PathsOf4<- function(g, n){
  Nb1<- NodeNeighbor1(g,n)
  Nb2<- c()
  for(i in Nb1){
   a<- NodeNeighbor1(g, i)
    a<- a[a!=n]
    if (length(a)==0){
      Nb1<- Nb1[Nb1!=i]}
    Nb2<- append(Nb2,a)
    Nb2<- unique(Nb2)
  }
  return(list(Nb1,Nb2))
}


PathsOf4b <- function(g,n){
  Nb1<- PathsOf4(g,n)[[1]]
  Nb2<- PathsOf4(g,n)[[2]]
  in1<- intersect(Nb1, Nb2)
  Nb2_search<- c()
  for (k in in1){Nb2_search<- Nb2[Nb2!=k]} #so as I dont search again the neighborhood of an element which has been considered already
  Nb3<- c()
  for(i in Nb2_search){
    b<- NodeNeighbor1(g,i)
    in2<- intersect(b, Nb1)
    for (j in in2){
      b<- b[b!=j]
    Nb3<- append(Nb3, b)
    Nb3<- unique(Nb3)
    }
  }
  tot_<- unique(append(append(Nb1, Nb2),Nb3))
  tot<-append(tot_, n)
  return(list(tot,Nb1,Nb2,Nb3))
}



g<- make_graph(edges=c(1,12,1,2,1,3,2,3,2,4,2,5,5,6,5,9,3,7,3,8,7,10,8,11),n=12, directed=FALSE)
tr<- make_graph(edges=c(1,12,1,2,1,3,2,4,2,5,2,6,5,9,3,7,3,8,7,10,8,11),n=12, directed=FALSE)



induced<- induced.subgraph(g,PathsOf4b(g,1)[[1]])
plot(induced)

###test on 
erd<- erdos.renyi.game(n=10, p.or.m = 0.5)
induced_erd<- induced.subgraph(erd,PathsOf4b(erd,1)[[1]])
plot(induced_erd)

bar <- barabasi.game(n=10, directed = FALSE)
induced_bar <- induced_subgraph(bar, PathsOf4b(bar, 1)[[1]])
plot(induced_bar)

## 1- The calculations for the third neighborhood are quite wrong.
## 2- For the erdos-renyi graphs the induced subgraphs are way more than those for barabasi, as the latter is sparsly connected. 




##### functions as written by me (NodeNeghbor1) and Zoran (configuration) in previous sessions
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




configuration <- function(deg_seq){
  stubs <-c()
  g <-make_empty_graph(length(deg_seq), directed = FALSE)
  for (i in 1:length(deg_seq)){
    stubs <-c(stubs, rep(i, deg_seq[i]))
  }
  used <-rep(0, length(stubs))
  for (i in 1:(length(stubs)/2)){
    s <-sample(which(used == 0), 2)
    used[s] <-1
    g <-add.edges(g, c(stubs[s[1]], stubs[s[2]]))
  }
  return(g)
}



