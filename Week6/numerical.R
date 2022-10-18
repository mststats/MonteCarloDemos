f=function(x) (cos(50*x)+sin(20*x))^2
curve(f, 0, 1)
optimise(f,c(0,1),maximum=TRUE)
optimise(f,c(0.4,1),maximum=TRUE) # actually slightly higher!!!
nlm(function(x) -f(x), 0) # terrible since it will get stuck in local modes

# uniroot also performs poorly sometimes
h=function(x){(x-3)*(x+6)*(1+sin(60*x))}
uniroot(h,int=c(-2,10))

