unif_rejection_irregular=function(n) {
  y=runif(n,0,1) # uniform proposal density
  M=4 # higher value than f(x) can take; not ideal since we can't know this!
  u=runif(n,0,1)
  accept = (u < (f(y)/M))
  x=y[accept]
  return(x) # accepted values only
}

# in the below, we have an additional "scale" term that is unused, other than to
# make sure we don't encounter a diverging grad estimate
stochastic_search=function(f, start, alpha=function(j) 1/(j+1), beta=function(j) 1/(j+1), 
                           maxiter=1e3, tol=1e-5) {
  theta=matrix(start,ncol=2)
  diff=iter=1
  for (iter in 1:maxiter){
    scale=2 # use this to check for diverging gradient estimates
    while (scale>1){ # always enter this loop at least once
      zeta=rnorm(2) # random directions
      zeta=zeta/c(sqrt(t(zeta)%*%zeta)) # normalise to have distance 1 i.e. a point on the unit sphere
      grad=alpha(iter)*zeta*(-f(theta[iter,]+beta(iter)*zeta)-
                               -f(theta[iter,]-beta(iter)*zeta))/(2*beta(iter))
      scale=sqrt(t(grad)%*%grad) # if scale>1 we have a diverging estimate of grad
      # if scale>1, try a new zeta and recalculate grad. Otherwise use the current one and exit this while loop
    }
    theta=rbind(theta,theta[iter,]+grad)
    diff=sqrt(t(grad)%*%grad)
    if (diff<tol) break
  }
  return(theta)
}


