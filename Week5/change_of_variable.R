# change of variable u=1/x
set.seed(12345)
R=1e3
h=function(u){ dnorm(1/u)/10/u^2 } # f(1/u)/(10u^2)
I=h(runif(R,0,1/10)) # 
est=cumsum(I)/(1:R) # Monte Carlo estimate for each value of i in 1 to n
err=sqrt(cumsum((I-est)^2))/(1:R) # sample standard error
plot(est,xlab="Iterations",ty="l",lwd=2,ylab="",
     ylim=c(min(est-2*err),max(est+2*err)))
lines(est+2*err,col=1,lwd=2,lty=2)
lines(est-2*err,col=1,lwd=2,lty=2)
integrate(h,0,1/10) # 7.47456e-26 with absolute error < 6.5e-27
abline(h=pnorm(-10),col=2,lwd=2) #7.619853e-24
legend("topright", col=c(1,1,2), lty=c(1,2,1), lwd=2, cex=1.5, 
       legend=do.call('expression',list(
       bquote(paste(hat(theta)[n],"(10)")),
       bquote(paste(hat(theta)[n],"(10) +/- 1.96 se",)),
       "truth=pnorm(-10)")))
       


