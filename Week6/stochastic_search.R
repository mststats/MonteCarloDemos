unif_rejection_irregular=function(n) {
  y=runif(n,0,1) # uniform proposal density
  M=4 # higher value than f(x) can take; not ideal since we can't know this!
  u=runif(n,0,1)
  accept = (u < (f(y)/M))
  x=y[accept]
  return(x) # accepted values only
}

stochastic_search=function(f, start, alpha=function(j) 1/(j+1), beta=function(j) 1/(j+1), 
                           maxiter=1e3, tol=1e-5) {
  theta=matrix(start,ncol=2)
  diff=iter=1
  for (iter in 1:maxiter){
    scale=2
    while (scale>1){
      zeta=rnorm(2)
      zeta=zeta/c(sqrt(t(zeta)%*%zeta))
      grad=alpha(iter)*zeta*(-f(theta[iter,]+beta(iter)*zeta)-
                               -f(theta[iter,]-beta(iter)*zeta))/beta(iter)
      scale=sqrt(t(grad)%*%grad)
    }
    theta=rbind(theta,theta[iter,]+grad)
    diff=sqrt(t(grad)%*%grad)
    if (diff<tol) break
  }
  return(theta)
}


