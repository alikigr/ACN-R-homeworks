library('igraph')


##ex1 
g<-random.graph.game(n=10, p.or.m = 0.4)



ValueAssortativity <- function(g){
  seqdeg <- degree(g)
  counter<-0
  Aobs<- assort(g)
  for (i in 1:100){ #B=100
    g_new<- configuration(seqdeg)
    Aperm<- assort(g_new)
    if (Aperm>=Aobs){
      counter<- counter+1
    }
  }
  emp_pvalue<- (counter+1)/(101) #B+1
  return(emp_pvalue)
}



##ex2

#I made my function to take as input a graph, calculate the degree of its nodes,
#then it samples two degrees randomly and then checks if the nodes with that degree, are adjacent (Aobs) (if more than one node has the given degree,
#then it takes the first one).
#afterwards, I make new graphs with the same sequence degree, I take the nodes with the degree chosen already, and I check the adjacency of them

g1<-random.graph.game(n=9, p.or.m = 0.4)

ValueAdjacency <-function(g){
  counter<- 0
  seqdeg<- degree(g)
  s<- sample(seqdeg,2)
  a<- which(seqdeg==s[1])[1]
  b<- which(seqdeg==s[2])[1]
  Aobs <- are_adjacent(g, V(g)[a], V(g)[b])
  for (i in 1:100){
    g_new <- configuration(seqdeg)
    Aperm<- are_adjacent(g_new, V(g_new)[a], V(g_new)[b])
    if (Aperm!=Aobs){
      counter <- counter+1
    }
  }
  emp_pvalue <- (counter+1)/(101)
  return(list(seqdeg,s,emp_pvalue))
}





##### functions as written by Zoran in the exercise sessions and used for ex1



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





