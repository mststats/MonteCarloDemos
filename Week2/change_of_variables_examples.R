# some examples of change of variables

# Suppose x is uniform on [0,1] so f_X(x)=1 when 0<=x<=1 and zero outside [0,1]
# If y=sqrt(x) => x=y^2 => dx/dy=2y
# change of variables: f_Y(y) = f_X(x)dx/dy = f_X(y^2)2y = 1*2y = 2y 
# F_Y(y)=y^2
# Monte Carlo check of this result:
x=runif(1e4) # 10,000 samples of x
y=sqrt(x) # samples of y
# compare empirical CDF and PDF to theoretical result from change of variables above
plot(ecdf(y), lwd=2); plot(function(y) y^2, from=min(y), to=max(y), add=TRUE, col=4, lwd=2) # F_Y(y)
hist(y,freq=FALSE,100); plot(function(y) 2*y, from=min(y), to=max(y), add=TRUE, col=4, lwd=2) # f_Y(y)

# second example
# Suppose x~Exp(lambda)  so f_X(x)=lambda*exp(-lambda*x) when 0<x<infinity 
# If y=exp(x) => x=log(y) => dx/dy=1/y 
# change of variables: f_Y(y) = f_X(x)dx/dy = f_X(log(y))/y = lambda*exp(-lambda*log(y))y^-1 = lambda*y^(-lambda-1)
# F_Y(y)=y^2
# Monte Carlo check of this result:
lambda=10
x=rexp(1e4,lambda) # 10,000 samples of x
y=exp(x) # samples of y
# compare empirical CDF and PDF to theoretical result from change of variables above
plot(ecdf(y), lwd=2); plot(function(y) -y^-lambda+1^-lambda, from=min(y), to=max(y), add=TRUE, col=4, lwd=2) # F_Y(y)
hist(y,freq=FALSE, 100); plot(function(y) lambda*y^(-lambda-1), from=min(y), to=max(y), add=TRUE, col=4, lwd=2) # f_Y(y)
