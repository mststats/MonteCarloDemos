beta_rejection=function(n,a,b) {
  y=runif(n,0,1) # uniform instrumental density
  M=dbeta(a/(a+b),a,b) # highest value f(x) can take
  u=runif(n,0,M) # 
  accept=u < (dbeta(y,a,b)/M)
  x=y[accept]
  return(x) # accepted values only
}