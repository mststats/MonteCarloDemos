R=1e5

# option a: roll two dice and take the absolute difference
die.1=sample(1:6, replace=TRUE, R)
die.2=sample(1:6, replace=TRUE, R)
MC.a=abs(die.1-die.2) 
E.MC.a=mean(MC.a) # about 70/36

# option b: toss a coin 4 times, count number of heads
coin.f=function(i) sample(c("H","T"), replace=TRUE, 4)
count.heads=function(i) sum(coin.f(i)=="H")
MC.b=sapply(1:R, count.heads) # sample from dbinom(x,4,prob=0.5)
E.MC.b=mean(MC.b) # about 2

# E[MC.a]<E[MC.b] and b wins about 43% of the time, versus 36% for a and 21% the draw
mean(MC.b>MC.a) # 43%
mean(MC.a>MC.b) # 36%
mean(MC.a==MC.b) # 21%