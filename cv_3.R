VraceniPenez <- function(M){
  Mince <- c(50,20,10,5,2,1)
  PocetMinci <- matrix(0,nrow=1,ncol=length(Mince))
  i <- 1
  while(M > 0){
    while (Mince[i]>M) {
      i = i +1
    }
    M = M-Mince[i]
    PocetMinci[i] <- PocetMinci[i] + 1
  }
  return(PocetMinci)
}
VraceniPenez(121)
