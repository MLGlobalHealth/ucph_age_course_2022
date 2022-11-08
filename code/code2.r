n=200
x=seq(0,1,length.out=n)
alpha=0.1
k1=function(x,alpha) exp(-x/alpha)
k2=function(x,alpha) exp(-x^2/alpha)
K1=k1(as.matrix(dist(x)),alpha)
K2=k2(as.matrix(dist(x)),alpha)

par(mfrow=c(2,2))
image(K1)
image(K2)

K1_ = t(chol(K1 + diag(1e-8,n)))
K2_ = t(chol(K2 + diag(1e-8,n)))

gamma = cbind(rnorm(n))

plot(x,K1_%*%gamma,type='l',col='red',lwd=5)
plot(x,K2_%*%gamma,type='l',col='blue',lwd=5)