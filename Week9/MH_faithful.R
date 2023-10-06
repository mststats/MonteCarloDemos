require(mvtnorm)
# mixture model with 3 components, inference via Gibbs sampling
data(faithful)
x=faithful[,1:2]
for (k in 1:2) 
  x[,k]=scale(x[,k])

MH_mixture=function(x, M, iter, prop.sd=c(1,1)) {
  accept=0
  n=nrow(x)
  d=ncol(x)
  mu=array(NaN,c(iter,M,d)) # mu[i,m,j] is mean in jth dimension of mth mixture at MCMC iteration i
  sigma=rep(NaN,iter) # note that this assumes spherical covariance matrices of equal size
  z=matrix(NaN,iter,nrow(x)) # mixture memberships
  for (m in 1:M) {
    mu[1,m,]=rmvnorm(1,rep(0,d), diag(1,d)) 
  }
  sigma[1]=0.3
  z[1,]=sample(1:M, n, replace=TRUE)
  z[1,]=kmeans(x,M)$cluster
  z.star=rep(NaN,n)
  mu.star=matrix(NaN,M,d)
  prop.var=prop.sd[2]^2
  
  log.q=function(t.mu, t.sigma, t.z)
    sum(replicate(n, function(j) dmvnorm(x[j,], t(t.mu[t.z[j],]), diag(t.sigma,d), log=TRUE))) - (n+1)*log(t.sigma)
  log.J=function(p.sigma, c.sigma)
    dgamma(p.sigma,(c.sigma^2)/prop.var,rate=c.sigma/prop.var,log=TRUE) 

  pb=txtProgressBar(min=1,max=iter)
  for (i in 2:iter) {
    setTxtProgressBar(pb, i)
    for (j in 1:n) {
      # to flip 1/(prop.sd[3]*M) of the memberships
      probs=rep(0,M);probs[z[i-1,j]]=(prop.sd[3]-1);probs=probs+1/M;probs=probs/sum(probs) 
      #probs=rep(1,M)
      z.star[j]=sample(1:M,1,prob=probs) 
    }
    for (m in 1:M) {
      mu.star[m,]=rmvnorm(1, t(mu[i-1,m,]), diag(prop.sd[1],d))
    }
    sigma.star=rgamma(1,(sigma[i-1]^2)/prop.var,rate=sigma[i-1]/prop.var)
    log.r=log.q(mu.star, sigma.star, z.star)-log.J(sigma.star, sigma[i-1])-
          log.q(mu[i-1,,], sigma[i-1], z[i-1,])+log.J(sigma[i-1], sigma.star)
    if (log(runif(1)) < log.r) {
      mu[i,,]=mu.star
      sigma[i]=sigma.star
      z[i,]=z.star
      accept=accept+1
    } else {
      mu[i,,]=mu[i-1,,]
      sigma[i]=sigma[i-1]
      z[i,]=z[i-1,]
    }
  }
  close(pb)
  cat("acceptance rate was ", 100*accept/iter, "%\n", sep="")
  return(list(mu,sigma,z))
}
