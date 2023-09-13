# mixture model with M mixture components
x=iris$Petal.Length  

MH_mixture=function(x, M, iter, prop.paras=c(1,1,1/20), init.z=kmeans(x,M)$cluster) {
  accept=0
  n=length(x)
  mu=matrix(NaN,iter,M) # mu[i,m,j] is mean in jth dimension of mth mixture at MCMC iteration i
  sigma=matrix(NaN,iter,M) 
  z=matrix(NaN,iter,n) # mixture memberships
  z[1,]=sample(1:M, n, replace=TRUE)
  z[1,]=init.z # start at very sensible mixture memberships
  mu[1,]=sapply(1:M, function(m) mean(x[z[1,]==m]))
  sigma[1,]=sapply(1:M, function(m) sd(x[z[1,]==m]))
  
  log.q=function(t.mu, t.sigma, t.z)
    sum(sapply(1:n, function(j) dnorm(x[j], t.mu[t.z[j]], t.sigma[t.z[j]], log=TRUE))) - sum(log(t.sigma))
  log.J=function(p.sigma, c.sigma)
    sum(sapply(1:M, function(m) dgamma(p.sigma[m],(c.sigma[m]^2)/prop.paras[2],rate=c.sigma[m]/prop.paras[2],log=TRUE)))

  pb=txtProgressBar(min=1,max=iter)
  for (i in 2:iter) {
    setTxtProgressBar(pb, i)
    for (j in 1:n) {
      # Gibbs update for mixture memberships / labels z
      probs=sapply(1:M, function(m) dnorm(x[j],mu[i-1,m],sigma[i-1,m]))
      probs=probs/sum(probs) # normalisation
      z[i,j]=sample(1:M,1,prob=probs) 
    }
    mu.star=mu[i-1,]+rnorm(M, 0, prop.paras[1])
    sigma.star=sapply(1:M, function(m) rgamma(1,(sigma[i-1,m]^2)/prop.paras[2],rate=sigma[i-1,m]/prop.paras[2]))
    log.r=log.q(mu.star, sigma.star, z[i-1,])-log.J(sigma.star, sigma[i-1,])-
          log.q(mu[i-1,], sigma[i-1,], z[i-1,])+log.J(sigma[i-1,], sigma.star)
    if (log(runif(1)) < log.r) { # accept and set samples to proposed values
      mu[i,]=mu.star
      sigma[i,]=sigma.star
      accept=accept+1
    } else { # reject and repeat sample values
      mu[i,]=mu[i-1,]
      sigma[i,]=sigma[i-1,]
      z[i,]=z[i-1,]
    }
  }
  close(pb)
  cat("acceptance rate was ", 100*accept/iter, "%\n", sep="")
  return(data.frame(mu=mu,sigma=sigma,z=z))
}
