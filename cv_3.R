VraceniPenez <- function(M,Mince){
  PocetMinci <- matrix(0,nrow=1,ncol=length(Mince))
  colnames(PocetMinci) <- Mince
  i <- 1
  while(M > 0){
    while (Mince[i]>M) {
      i = i +1
      if(i>length(Mince)){
        i = 1
        break
      }
    }
    M = M-Mince[i]
    if(M>0){
      PocetMinci[i] <- PocetMinci[i] + 1
    }
  }
  return(PocetMinci)
}
VraceniPenez(153,c(500,22,2))
