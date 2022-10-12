control_variates_x8=function(R){
  x.pilot=rnorm(R/10) # 10% budget for this
  h=function(x) x^4-3 # E[x^4]=3
  phi=function(x) x^8
  coefs=lm(phi(x.pilot)~0+h(x.pilot))$coef
  x=rnorm(R)
  samps=phi(x) - coefs[1]*h(x) 
  return(samps)
}

control_variates_pnorm=function(R,upp) {
  x.pilot=rnorm(R/10) # use 10% of our budget
  h1=function(x) x-0 # E[X]=0
  h2=function(x) x^2-1 # E[X^2]=1
  phi=function(x) x>upp
  coefs=lm(phi(x.pilot)~0+h1(x.pilot)+h2(x.pilot))$coef
  x=rnorm(R)
  samps=phi(x) - coefs[1]*h1(x) - coefs[2]*h2(x)
  return(samps)
}