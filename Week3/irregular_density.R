# density as per Fig 2.4 of Monte Carlo Statistical Methods
f=function(x){ 
  exp(-x^2/2)*(sin(6*x)^2+3*cos(x)^2*sin(4*x)^2+1)
}

# proposal density is proportional to a N(0,1) density
h=function(y){ exp(-y^2/2)/sqrt(2*pi)}

envelope_rejection_irregular=function(n) {
  # find the largest value f(x)/h(x) can take
  M=optimise(function(x) f(x)/h(x),c(-2*pi,2*pi),maximum=TRUE)$objective
  y=rnorm(n) # sample from proposal
  u=runif(n,0,M) # sample uniforms
  accept=u < (f(y)/h(y)) # accept / reject step
  x=y[accept] # keep only accepted values
  return(x)
}
