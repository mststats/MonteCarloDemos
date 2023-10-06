beta_rejection=function(n,a,b) {
  y=runif(n,0,1) # uniform proposal density
  # mode is (a-1)/(a+b-2) provided a,b>1
  M=dbeta((a-1)/(a+b-2),a,b) # highest value f(x) can take
  u=runif(n,0,1)  
  accept=u*M < (dbeta(y,a,b))
  x=y[accept]
  return(x) # accepted values only
}

# next version uses density up to constant 1/Beta(a,b); twice as fast!
beta_rejection_simpler=function(n,a,b) {
  cdbeta=function(x,a,b) exp((a-1)*log(x) + (b-1)*log(1-x))
  y=runif(n,0,1) # uniform proposal density
  # mode is (a-1)/(a+b-2) provided a,b>1
  M=cdbeta((a-1)/(a+b-2),a,b) # highest value f(x) can take
  u=runif(n,0,1)  
  accept=u*M < (cdbeta(y,a,b))
  x=y[accept]
  return(x) # accepted values only
}

# envelope method with different beta as proposal density  (needs greater variance)
beta_rejection_envelope=function(n,a,b) {
  h=function(x) dbeta(x,a/2,b/2)
  f=function(x) dbeta(x,a,b)
  # find the highest value f(x)/h(x) can take
  M=optimize(function(x) f(x)/h(x), maximum=TRUE, interval=c(0,1))$object
  y=rbeta(n,a/2,b/2) # beta proposal density
  u=runif(n,0,1) 
  accept=u < (f(y)/(M*h(y)))
  x=y[accept]
  return(x) # accepted values only
}


