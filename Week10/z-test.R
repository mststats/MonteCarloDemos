# Monte Carlo version H0 is that mean is m
MC.ztest=function(x,K=1e3,m=0,s=1) {# K is number of Monte Carlo sims per dataset
  n=length(x)
  stat.sim=rep(NaN,K)
  obs=mean(x)
  for (k in 1:K) {
    sims.x=rnorm(n,m,s) # sample from H0
    stat.sim[k]=mean(sims.x) # Monte Carlo sample mean for simulation k
  }
  p.value=mean(abs(stat.sim)>abs(obs)) # fraction of simulated stats more extreme than observed one
  return(list(obs=obs, MC.t=stat.sim, p.value=p.value))
}

