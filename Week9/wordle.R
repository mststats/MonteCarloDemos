N=269929 # number of tries shared on twitter on 24/01/2022
tries=c(1:7)
probs=c(0.01,0.05,0.28,0.38,0.20,0.07,0.01) # not quite sum to 1!

# we don't get to observe how many tries it would have taken them, so this is missing
n=round(N*probs)
max.z=16 # z will be number of 7,8,...,max.z tries 
a=16/10;b=4/10

n.f=n[7] # number who failed that day
n=n[1:6] # only 1 to 6 can actually be observed

Gibbs_wordle=function(iter){
  lambda=rep(NaN,iter)
  z=matrix(NaN,iter,length(7:max.z)) # 7, ..., 16 tries 
  colnames(z)=7:max.z
  lambda[1]=tries%*%probs # sample mean
  tmp=rpois(1.5*n.f/(1-ppois(6,lambda[1])), lambda[1]);tmp=tmp[tmp>6];tmp=tmp[1:n.f]
  z[1,]=sapply(7:max.z, function(g) sum(tmp==g))
  pb=txtProgressBar(min=1,max=iter)
  for (i in 2:iter) {
    setTxtProgressBar(pb, i)
    lambda[i]=rgamma(1,a+(1:6)%*%n+(7:max.z)%*%z[i-1,], b+N)
    tmp=rpois(1.5*n.f/(1-ppois(6,lambda[i])), lambda[1]);tmp=tmp[tmp>6];tmp=tmp[1:n.f] # expect n.f to be above 6
    z[i,]=sapply(7:max.z, function(g) sum(tmp==g))
  }
  close(pb)
  return(data.frame(lambda,z))
}
