n=200
x=seq(0,1,length.out=n)
alpha=0.1
k1=function(x,alpha) exp(-x/alpha)
K1=k1(as.matrix(dist(x)),alpha)
K1_ = t(chol(K1 + diag(1e-8,n)))
gamma = cbind(rnorm(n))

z=K1_%*%gamma
f=lm.fit(x=K1_,y=z)

par(mfrow=c(2,1))
plot(z,f$fitted.values,pch=16,cex=1)
abline(0,1,col='red')
plot(gamma,f$coefficients,pch=16,cex=1)
abline(0,1,col='red')
