gibbs_gen <- function(n, y)
{
  z.o <- 20
  th.o <- 0.5
  z <- z.o
  for(j in 1:n){
    th <- rbeta(1,z+y[4]+1, y[2]+y[3]+1)
    p <- th/(2 + th)
    z <- rbinom(1, y[1], p)
    z.o <- c(z.o, z)
    th.o <- c(th.o, th)
  }
  cbind(th.o, z.o)
}
