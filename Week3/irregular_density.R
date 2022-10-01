# density as per Fig 2.4 of Monte Carlo Statistical Methods
f=function(x){ 
  exp(-x^2/2)*(sin(6*x)^2+3*cos(x)^2*sin(4*x)^2+1)
}

# proposal density is N(0,1) 
h=function(y) dnorm(y,0,1)

envelope_rejection_irregular=function(n) {
  # find the largest value f(x)/h(x) can take
  M=optimise(function(x) f(x)/h(x),c(-2*pi,2*pi),maximum=TRUE)$objective
  y=rnorm(n) # sample from proposal
  u=runif(n,0,1) # sample uniform
  accept=u < (f(y)/(M*h(y))) # accept / reject step
  x=y[accept] # keep only accepted values
  return(x)
}
