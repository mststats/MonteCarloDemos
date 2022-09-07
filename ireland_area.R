# estimate the area of an island
require(maps)
require(geosphere)
xrange=c(-11,-6)
yrange=c(51,56)
par(bty='n')
plot(xrange,yrange,xlab="long",ylab="lat",t='n')
ll.m=map(regions="Ireland",fill=TRUE,add=TRUE)
p=cbind(xrange[c(1,2,2,1,1)],yrange[c(1,1,2,2,1)])
lines(p) # draw a box around it
R=1e5
x=runif(R,xrange[1],xrange[2])
y=runif(R,yrange[1],yrange[2])
check.coords=map.where("world",x,y,proj="bonne",param=45)
check.coords[is.na(check.coords)]="sea"
points(x,y,pch=20,col=c(8,2)[1+1*(check.coords=="Ireland")])
lines(ll.m)
# area is then area of square by proportion of samples in Ireland, scaled to km
MC.area=sum(check.coords=="Ireland",na.rm=TRUE)/R*areaPolygon(p)*1e-6 # polygon area of p is m^2
# 
# verify this value
b.m=map(regions="Ireland",proj="bonne",param=45,fill=TRUE,plot=FALSE)
area.map(b.m, "Ireland")*1.60934^2 # Rep of Ireland is about 68964.88 km^2
