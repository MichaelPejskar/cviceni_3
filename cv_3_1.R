#Michael Pejskar PRG cv 3
rm(list = ls())
Cokolada <- function(M,x,y){
  C=0
  if(x>=nrow(M) | y>=ncol(M)){
    C = C + M[x,y]
    return(C)
  }
  else{
    C = M[x,y]
    Cdolu = Cokolada(M,x+1,y)
    Csikmo = Cokolada(M,x+1,y+1)
    C = C + max(C,Cdolu,Csikmo)
    print(C)
  }
  return(C)
}
M=matrix(0,nrow=3,ncol=3)
M[1,] = c(1,5,2)
M[2,] = c(3,0,5)
M[3,] = c(0,8,7)
Cokolada(M,1,2)

CokoIter <- function(M){
  s <- dim(M)
  k1 <- seq(from=s[1]-1,to=1,by=-1)
  for(r in k1){
    k2 <- seq(from=r, to=1,by=-1)
    for(s in k2){
      Cdolu <- M[r+1,s]+M[r,s]
      Csikmo <- M[r+1,s+1]+M[r,s]
      M[r,s] <- max(c(Cdolu,Csikmo))
    }
  }
  return(M[1,1])
}
CokoIter(M)
