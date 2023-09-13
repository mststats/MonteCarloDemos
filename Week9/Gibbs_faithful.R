require(mvtnorm)
# mixture model with 3 components, inference via Gibbs sampling
data(faithful)
x=faithful[,1:2]
for (k in 1:2) 
  x[,k]=scale(x[,k])

gibbs_mixture=function(x, M, iter, init=kmeans(x,M)$cluster) {
  n=nrow(x)
  d=ncol(x)
  mu=array(NaN,c(iter,M,d)) # mu[i,m,j] is mean in jth dimension of mth mixture at MCMC iteration i
  sigma=rep(NaN,iter) # note that this assumes spherical covariance matrices of equal size
  z=matrix(NaN,iter,nrow(x)) # mixture memberships
  z[1,]=init
  sigma[1]=1/rgamma(1,(n+1)/2,sum((t(x)-colMeans(x[z[1,],]))^2)/2)
  for (m in 1:M) {
    mu[1,m,]=rmvnorm(1,colSums(x[z[1,]==m,])/sum(z[1,]==m), diag(sigma[1]/sqrt(sum(z[1,]==m)),d)) #works
  }
  probs=matrix(NaN, n, M) # prob of z values
  pb=txtProgressBar(min=1,max=iter)
  for (i in 2:iter) {
    setTxtProgressBar(pb, i)
    for (j in 1:n) {
      for (m in 1:M)
        probs[j,m]=exp(-sum((t(x[j,])-mu[i-1,m,])^2)/(2*sigma[i-1]))
      probs[j,]=probs[j,]/sum(probs[j,])
    }
    for (j in 1:n)
      z[i,j]=sample(1:M,1,prob=probs[j,])
    sigma[i]=1/rgamma(1,(n+1)/2,sum((t(x)-t(mu[i-1,z[i,],]))^2)/2)
    for (m in 1:M) {
      mu[i,m,]=rmvnorm(1,colSums(x[z[1,]==m,])/sum(z[i,]==m), diag(sigma[i]/sqrt(sum(z[i,]==m)),d)) #works
    }
  }
  close(pb)
  return(list(mu,sigma,z))
}
