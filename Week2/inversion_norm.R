m=0;s=1 # mean and standard deviation for the demo
par(mfrow=c(2,2))
set.seed(12346)
#R=1e3
u=x=rep(NaN,R)
for (i in 1:R) {
  u[i]=runif(1)
  x[i]=qnorm(u[i],m,s)
}
  # now plot the CDF 
  plot(ecdf(x), verticals=TRUE, do.points=TRUE, col=8, xlab="X", ylab="F(X)")
  plot(function(y) pnorm(y,m,s), qnorm(0.001,m,s), qnorm(0.999,m,s), lwd=2, add=TRUE)
  legend("topleft", c("Normal CDF","x samples CDF"),lty=1,col=c(1,8),pch=c(-1,20))
  # now sample dists for u and x
  hist(u,freq=FALSE)
  hist(x,freq=FALSE,ylim=c(0,dnorm(m,m,s)))
  plot(function(y) dnorm(y,m,s),qnorm(0.001,m,s),qnorm(0.999,m,s),add=TRUE,lwd=2)
  plot(1:10, t='n',axes=F,xlab="",ylab="")
  text(5,y=5,paste(R, "samples"),cex=2.5)