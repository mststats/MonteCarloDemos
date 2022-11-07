# Metropolis Algorithm for Poisson lambda=log(rate) given counts vector x
Metropolis=function(x,init,iter) {
  n=length(x)
  lambda=rep(NA,iter)
  lambda[1]=init
  prop.sd=1.5
  acc.count=0
  q=function(t.lambda)
    exp(-n*exp(t.lambda)+sum(x)*t.lambda-0.25*t.lambda^2) 
  pb=txtProgressBar(min=1,max=iter)
  print("Metropolis:")
  for(i in 2:iter) {
    setTxtProgressBar(pb, i)
    lambdastar=rnorm(1,lambda[i-1],prop.sd)
    r=q(lambdastar) / q(lambda[i-1])
    # it can be more stable to use the log of the acceptance ratio:
    #logr=(-n*exp(lambdastar)+sum(x)*lambdastar-0.25*lambdastar^2) - (-n*exp(lambda[i-1])+sum(x)*lambda[i-1]-0.25*lambda[i-1]^2)
    # we'd then need to check if log(U)<logr
    U=runif(1)
    if(U<r) {
      lambda[i]=lambdastar 
      acc.count=acc.count+1
    } else {
      lambda[i]=lambda[i-1] 
    }
  }
  close(pb)
  cat("accepted ", signif(100*acc.count/iter,2), "%\n",sep="")
  return(lambda)
}
x=c(3,5,2,3,6,1,0,1,0,1) # sum(x)=22
lambda=Metropolis(x,0,3e4) # one chain of samples
# create lists of mcmc objects; each object is a Markov Chain of samples
# purposefully start each chain at a different initial value; e.g. log(chain index)
require(coda) # to change each chain to an mcmc class object for use with gelman.plot and gelman.diag
lambda.chains=lapply(1:4, function(i) as.mcmc(Metropolis(x,log(i),1e4))) 

