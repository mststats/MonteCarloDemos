# congruential generator RNG
congruent_modulo_RNG=function(seed,a,b,M,R) {
  randvector=rep(NaN,R)
  for (i in 1:R){
    randvector[i]=(a*seed+b)%%M
    seed=randvector[i]
  }
  return(randvector/M)
}
