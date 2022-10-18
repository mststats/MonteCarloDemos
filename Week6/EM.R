x=c(125,20,18,34)
n=sum(x)
start=EM=theta=diff=.01
while (diff>.001){ #stopping rule
  EM=c(EM,((theta*x[1]/(2+theta))+x[4])/((theta*x[1]/(2+theta))+x[2]+x[3]+x[4]))
  diff=abs(theta-EM[length(EM)])
  theta=EM[length(EM)]
}

# MCEM version
M=100 # number of sims of z per iteration
MCEM=matrix(start,ncol=length(EM),nrow=500)
for (i in 2:length(EM)){
  z=rbinom(500,M*x[1],prob=MCEM[,i-1]/(2+MCEM[,i-1]))
  MCEM[,i]=(z/M+x[4])/(z/M+x[2]+x[3]+x[4])
}
