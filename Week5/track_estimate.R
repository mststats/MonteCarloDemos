# code to plot the Monte Carlo estimate and standard errors as a function of sample size based on n samples
track_estimate=function(samps,col="black"){
    R=length(samps)
    est=cumsum(samps)/1:R # running sample mean
    err=sqrt(cumsum((samps-est)^2))/1:R # running s.e.
    plot(est,t='l',ylim=c(min(est-2*err),max(est+2*err)))
    lines(est-1.96*err,col=col,lty=2)
    lines(est+1.96*err,col=col,lty=2);
}
