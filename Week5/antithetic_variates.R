antithetic_variates_x8=function(R){
  phi=function(x) x^8
  x=rnorm(R)
  est=(phi(x)+phi(-x))/2
  return(est)
}

antithetic_variates_pnorm=function(R,upp) {
  phi=function(x) x>upp
  u=runif(R)
  est=(phi(qnorm(u))+phi(qnorm(1-u)))/2
  return(est)
}