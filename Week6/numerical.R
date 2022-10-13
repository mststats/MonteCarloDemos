f=function(x) (cos(50*x)+sin(20*x))^2
curve(f, 0, 1)
optimise(f,c(0,1),maximum = TRUE)
nlm(function(x) -f(x), 0) # terrible since it will get stuck in local modes

# stochastic search
u=runif(1e3,0,1)
r.f=f(u)
i=which.max(r.f)

unif_rejection_irregular=function(n) {
  y=runif(n,0,1) # uniform proposal density
  M=4 # higher value than f(x) can take; not ideal since we can't know this!
  u=runif(n,0,1)
  accept = (u < (f(y)/M))
  x=y[accept]
  return(x) # accepted values only
}

x=unif_rejection_irregular(1e5)
r.f=f(x)
i=which.max(r.f)
x[i]
r.f[i]



