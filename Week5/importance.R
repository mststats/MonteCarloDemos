# Normal CDF approximation using inverted uniform density as importance function
R=1e4
u=runif(R,0,1/10)
x=1/u # f(u)=10 when u~[0,1/10] => g(x)=f(1/u)du/dx = 10/x^2
weit=dnorm(x)/(10/x^2) # f(x)/g(x)
estint=cumsum(weit)/1:R
se=sqrt(cumsum((weit-estint)^2))/(1:R) # sample standard error
plot(estint,t='l')
lines(estint-1.96*se,lty=2,t='l')
lines(estint+1.96*se,lty=2,t='l')
abline(h=pnorm(-10),col=2)

importance_inverse_uniform=function(R,z){
  g=function(x) z/x^2 #  f(1/u)du/dx = z/x^2 when u=1/x is uniform in 0,1/z
  x=1/runif(R,0,1/z)
  w=dnorm(x)/g(x) 
  phi=1 # always 1 as it's an indicator that x>z which g(x) only samples from
  return(phi*w)
}

# function to demonstrate E[x^8] where x is N(0,1)
importance_x8=function(R, s=2){
  x=rnorm(R)
  est=cumsum(x^8)/1:R # running mean
  err=sqrt(cumsum((x^8-est)^2))/1:R # running s.e.
  plot(est,t='l',col=2,lwd=2,ylim=c(min(est-2*err),max(est+2*err)))
  lines(est-1.96*err,col=2,lty=2);lines(est+1.96*err,col=2,lty=2);
  # now repeat using importance function N(0,s)
  xg=x*s # use the same samples and adjust the variance
  weit=dnorm(xg)/dnorm(xg,0,s) # f(x)/g(x)
  phi=xg^8
  estg=cumsum(phi*weit)/1:R # running mean
  errg=sqrt(cumsum((xg^8*weit-estg)^2))/1:R # running s.e.
  lines(estg,t='l',col=4);lines(estg-1.96*errg,col=4,lty=2);lines(estg+1.96*errg,col=4,lty=2);
  abline(h=105) # true expected value of x^8 is 105*sd(x)^8
}
