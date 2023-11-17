# example 5.8 
h=function(x) (cos(50*x)+sin(20*x))^2
H=function(x,temp) exp(f(x)/temp)
temp=function(j) 0.1/log(j+1)
x=runif(1)
hval=hcur=h(x)
diff=iter=1
while (diff>1e-4) {
  scale=sqrt(temp(iter))
  prop=x[iter]+runif(1,-1,1)*scale # range of x is [0,1] so use uniform proposals 
  if ((prop>1) || (prop<0) || (log(runif(1))*temp(iter)>h(prop)-hcur))
    prop=x[iter]
  x=c(x,prop)
  hcur=h(prop)
  hval=c(hval,hcur)
  if ((iter>10) && (length(unique(x[(iter/2):iter]))>1))
    diff=max(hval)-max(hval[1:(iter/2)])
  iter=iter+1
}

# next uses da mixture data again, simulated annealing to find MLE(mu)
SAmix=function (x, tolerance = 10^(-4), factor = 1) 
{
  like = function(mu) {
    -sum(log((0.25 * dnorm(da - mu[1]) + 0.75 * dnorm(da - mu[2]))))
  }
  scale = iter = dif = 1
  theta = matrix(x, ncol = 2)
  curlike = hval = like(x)
  while (dif > tolerance) {
    prop = theta[iter, ] + rnorm(2) * scale[iter]
    if ((max(-prop) > 2) || (max(prop) > 5) || (temp(iter) * 
                                                log(runif(1)) > -like(prop) + curlike)) 
      prop = theta[iter, ]
    curlike = like(prop)
    hval = c(hval, curlike)
    theta = rbind(theta, prop)
    iter = iter + 1
    ace = length(unique(theta[(iter/2):iter, 1]))
    if (ace == 1) 
      factor = factor/10
    if (2 * ace > iter) 
      factor = factor * 10
    scale = c(scale, max(2, factor * sqrt(temp(iter))))
    dif = (iter < 100) + (ace < 2) + (max(hval) - max(hval[1:(iter/2)]))
  }
  list(theta = theta, like = hval, ite = iter)
}

