n=m=50
x=rnorm(n,1)
y=rnorm(m)
xy=c(x,y) # all samples together
res=ks.test(x,y)
res.MC=ks.test(x,y,simulate.p.value=TRUE,B=99) # plus the observed labels means 100 values total

Fx=ecdf(x) # this is then a function that returns the CDF_x of any value
Fy=ecdf(y) # this is then a function that returns the CDF_y of any value
D=max(abs(Fx(xy)-Fy(xy))) # largest distance between Fx and Fy for any value in either sample
at=which.max(abs(Fx(xy)-Fy(xy))) # location of largest distance between Fx and Fy 
