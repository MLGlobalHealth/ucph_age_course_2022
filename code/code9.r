set.seed(123)
n=500
x= cbind(runif(n),runif(n))
x=scale(x)
alpha=0.5
m=100
d=as.matrix(dist(x))
K1<-exp(-0.5*d*d/(alpha^2))
Omega = cbind(rnorm(m),rnorm(m)) # Squared exponential kernel
Proj = x %*% t(Omega) # Projection - combine data with sample frequencies
Phi = sqrt(1/m) * cbind(cos(Proj/alpha), sin(Proj/alpha)) # Fourier feature matrix
K = Phi %*% t(Phi) # approximation of Kernel matrix
par(mfrow=c(1,2))
plot(K1,K)
abline(0,1,col='red')
library(randtoolbox)
Omega_qmc<-t(qnorm(halton(m,2)))
Omega_qmc = cbind(rnorm(m),rnorm(m)) # Squared exponential kernel
Proj = x %*% t(Omega) # Projection - combine data with sample frequencies
Phi = sqrt(1/m) * cbind(cos(Proj/alpha), sin(Proj/alpha)) # Fourier feature matrix
K2 = Phi %*% t(Phi) # approximation of Kernel matrix
plot(K2,K)
abline(0,1,col='red')
