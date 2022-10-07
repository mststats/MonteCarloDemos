importance_cauchy=function(R){
  g=function(x) 2/x^2 # integrate(g,2,Inf)=1 with absolute error < 4.2e-11
  # since G(x)=1-2/x we have G^-1(u) = 2/(1-u) or 2/u
  x=2/runif(R) # simulation of g(x) via method of inversion
  weit=dcauchy(x)/g(x)
  phi=1
  return(phi*weit)
 }