iter=1e4
x=c(3,5,2,3,6,1,0,1,0,1) # sum(x)=22
n=length(x)

# Metropolis Algorithm
lambda=rep(NA,iter)
lambda[1]=0
prop.sd=1.5
prop.var=3*(prop.sd^2)
acc.count=0
pb=txtProgressBar(min=1,max=iter)
print("Metropolis:")
for(i in 2:iter) {
  setTxtProgressBar(pb, i)
  lambdastar=rnorm(1,lambda[i-1],prop.sd)
  r=exp(-n*exp(lambdastar)+sum(x)*lambdastar-0.25*lambdastar^2) /  exp(-n*exp(lambda[i-1])+sum(x)*lambda[i-1]-0.25*lambda[i-1]^2)
  #logr=(-n*exp(lambdastar)+sum(x)*lambdastar-0.25*lambdastar^2) - (-n*exp(lambda[i-1])+sum(x)*lambda[i-1]-0.25*lambda[i-1]^2)
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
