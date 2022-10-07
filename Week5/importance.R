# Normal CDF approximation using uniform density as importance function
R=1e4
y=runif(R,0,1/10)
weit=dnorm(1/y)*10 # f(x)/g(x)
estint=cumsum(weit)/1:R
se=sqrt(cumsum((weit-estint)^2))/(1:R) # sample standard error
plot(estint,t='l')
lines(estint-1.96*se,lty=2,t='l')
lines(estint+1.96*se,lty=2,t='l')
abline(h=pnorm(-10),col=2)

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

track_estimate=function(samps,col="black"){
  R=length(samps)
  est=cumsum(samps)/1:R # running sample mean
  err=sqrt(cumsum((samps-est)^2))/1:R # running sample standard error
  plot(est,t='l',lwd=2,ylim=c(min(est-2*err),max(est+2*err)),xlab="n",ylab="Monte Carlo Estimate")
  lines(est-1.96*err,col=col,lty=2);lines(est+1.96*err,col=col,lty=2);
  legend("topright", col=col, lty=c(1,2), lwd=2, cex=1.5, 
         legend=do.call('expression',list(
         bquote(paste(hat(theta)[n])),
         bquote(paste(hat(theta)[n](f)," +/- 1.96 se",)))))
}