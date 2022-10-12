antithetic_variates_pnorm=function(R,upp) {
  phi=function(x) x>upp # phi(qnorm(u)) is monotone in u
  u=runif(R) 
  samps=(phi(qnorm(u))+phi(qnorm(1-u)))/2
  return(samps)
}