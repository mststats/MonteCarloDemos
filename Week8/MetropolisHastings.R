# Metropolis-Hastings Algorithm for Poisson rate gamma given counts data x
MH=function(x, init, iter) {
  n=length(x)
  prop.sd=1.5
  prop.var=3*(prop.sd^2)
  q=function(t.gamma) 
    t.gamma^(sum(x)-1) * exp(-n*t.gamma - 0.25*log(t.gamma)^2) 
  j=function(p.gamma,c.gamma)
    dgamma(p.gamma,(c.gamma^2)/prop.var,rate=c.gamma/prop.var)
  gammat=rep(NA,iter)
  gammat[1]=init
  acc.count=0
  pb=txtProgressBar(min=1,max=iter)
  print("Metropolis-Hastings:")
  for(i in 2:iter) {
    setTxtProgressBar(pb, i)
    gammastar=rgamma(1,(gammat[i-1]^2)/prop.var,rate=gammat[i-1]/prop.var)
    r=(q(gammastar)/j(gammastar,gammat[i-1])) / (q(gammat[i-1])/j(gammat[i-1],gammastar)) 
    U=runif(1)
    if(U<r) {
      gammat[i]=gammastar 
      acc.count=acc.count+1
    } else {
      gammat[i]=gammat[i-1] 
    }
  }
  close(pb)
  cat("accepted ", signif(100*acc.count/iter,2), "%\n",sep="")
  return(gammat)
}

x=c(3,5,2,3,6,1,0,1,0,1) # sum(x)=22
gammat=MH(x,1,3e4) # one chain of samples
# create lists of mcmc objects; each object is a Markov Chain of samples
# purposefully start each chain at a different initial value; e.g. chain index
require(coda) # to change each chain to an mcmc class object for use with gelman.plot and gelman.diag
gammat.chains=lapply(1:4, function(i) as.mcmc(MH(x,i,1e4)))
