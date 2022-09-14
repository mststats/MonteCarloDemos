inversion = function(n,inv.func) {
  u = runif(n)
  inv.func(u)
}

inv.exp = function(u,lambda=1){
  -(log(1-u))/lambda 
}

inv.max.exp=function(u,m,lambda=1){
  -1/lambda*log(1-u^(1/m))
}

inv.min.exp=function(u,m,lambda=1){
  -log(1-u)/(m*lambda)
}