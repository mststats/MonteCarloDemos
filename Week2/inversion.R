inversion = function(n,inv.func) {
  u = runif(n)
  inv.func(u)
}

inv.exp = function(u,lambda=1){
  -(log(1-u))/lambda 
  }