iter=1e4
x=c(3,5,2,3,6,1,0,1,0,1) # sum(x)=22
n=length(x)

###########
# Metropolis-Hastings Algorithm
gammat=rep(NA,iter)
gammat[1]=1
acc.count=0
pb=txtProgressBar(min=1,max=iter)
print("Metropolis-Hastings:")
for(i in 2:iter) {
  setTxtProgressBar(pb, i)
  gammastar=rgamma(1,(gammat[i-1]^2)/prop.var,rate=gammat[i-1]/prop.var)
  r=( ( gammastar^(sum(x)-1) * exp(-n*gammastar - 0.25*log(gammastar)^2) ) / dgamma(gammastar,(gammat[i-1]^2)/prop.var,rate=gammat[i-1]/prop.var) ) /
       ( ( gammat[i-1]^(sum(x)-1) * exp(-n*gammat[i-1] - 0.25*log(gammat[i-1])^2) ) / dgamma(gammat[i-1],(gammastar^2)/prop.var,rate=gammastar/prop.var) )
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