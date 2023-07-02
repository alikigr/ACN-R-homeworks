geia<- matrix(1:9,nrow=3, ncol=3)
mt<- matrix(c(1,2,3,2,1,2,3,2,1),3,3)


#exercise1
SumOfColumns<- function(mat){
  nc <- ncol(mat)
  for(i in 1:nc){
    a<- sum(mat[,i]);
    print(a)}}


#exercise2
MatSymmetry1 <- function(mat){
  symmetry<- NULL
  r<-nrow(mat)
  c<-ncol(mat)
  for(i in 1:(c-1)){
    for(j in (i+1):r){
      if(mat[i,j]==mat[j,i]){
        symmetry<-TRUE
      } else{symmetry<-FALSE}
    }
  }
  return(symmetry)
}


  
MatSymmetry2 <- function(mat){
  symmetry <- NULL
  if (all(mat==t(mat))){
    symmetry <- TRUE
  }
  else{ symmetry <- FALSE}
  return(symmetry)
}




##exercise3
#matrix multiplication: mat1%*%mat2

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



#test
SumOfColumns(mt)
MatSymmetry1(mt)
MatSymmetry1(geia)
MatrixPower(mt, 2)
