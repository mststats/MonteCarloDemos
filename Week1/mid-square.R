# von Neumann's mid-sqaure algorithm
midSquareRand=function(seed, R) {
  n=nchar(seed)
  take.n=ifelse(n%%2==0,n,n+1)
  randvector=rep(NaN,R)
  for(i in 1:R) {
    value=seed^2
    X=as.numeric(unlist(strsplit(as.character(value),split="")))
    if (length(X)<(2*n)) X=c(rep(0, 2*n-length(X)), X) # add 0s to the left if needed so that length is always >= 2n
    P=X[((2*n-take.n)/2+1):((2*n-take.n)/2+take.n)]
    seed=as.numeric(paste(P,collapse= ""))
    randvector[i]=seed
  }
  return(randvector)
}
