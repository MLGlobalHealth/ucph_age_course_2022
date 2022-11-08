n=100
x=seq(0,1,0.001)

K1=function(x,alpha) exp(-x/alpha)
K2=function(x,alpha) exp(-x^2/alpha)

alpha=0.1
par(mfrow=c(1,2))
plot(x,K1(x,alpha),type='l',lwd=5)
plot(x,K2(x,alpha),type='l',lwd=5)