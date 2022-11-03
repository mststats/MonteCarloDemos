# Gibbs Exercise 1

iter <- 5e3
n <- 12
xbar <- 119
theta0 <- 110
phi0 <- 20
S <- 13045
alpha = 6
beta = 1000

theta <- rep(NA,length=iter)
phi <- rep(NA,length=iter)

theta[1] <- 0
phi[1] <- 1

pb=txtProgressBar(min=1,max=iter,style=3)
for(i in 2:iter) {
  phi1 <- 1/((1/phi0)+(n/phi[i-1]))
  theta1 <- phi1*(theta0/phi0+xbar*n/phi[i-1]) 
  theta[i] <- rnorm(1,mean=theta1,sd=sqrt(phi1))
  phi[i] <- 1/rgamma(1,(n-1)/2+alpha,0.5*(S+n*(xbar-theta[i])^2+beta)) # checked and correct
  setTxtProgressBar(pb, i)
}
close(pb)
