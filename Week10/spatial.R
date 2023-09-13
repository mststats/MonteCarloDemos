require(splancs)
MC.spatialtest=function(x,K=1e3){
  n=nrow(x)
  t.obs=sum(nndistG(x)$dists)
  t.sim = rep(0,K)
  for (k in 1:K){
    y.coord = runif(2*n)
    y = matrix(y.coord,n,2)
    t.sim[k] = sum(nndistG(y)$dists)
  }
  p.value.less=mean(abs(t.obs)<abs(t.sim))
  p.value.more=mean(abs(t.obs)>abs(t.sim))
  p.value=min(p.value.less,p.value.more)
  return(list(t.obs=t.obs,t.sim=t.sim,p.value=p.value))
}
