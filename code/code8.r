set.seed(123)
n=500
x=seq(0,1,length.out=n)
alpha=0.1
k1=function(x,alpha) exp(-x/alpha)
K1=k1(as.matrix(dist(x)),alpha)
K1_ = t(chol(K1 + diag(1e-8,n)))
gamma = cbind(rnorm(n))
z=K1_%*%gamma 
z_= z+ rnorm(n,0,1)
f=lm.fit(x=K1_,y=z_)
plot(x,z_,pch=16,cex=2)
lines(x,z,col='blue',lwd=2)
library(glmnet)
lambdas <- 10^seq(5, -5, length.out=100)
cv_fit <- cv.glmnet(K1_, z_, alpha = 0, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min
fit <- glmnet(K1_, z_, alpha = 0, lambda = opt_lambda)
f2 <- predict(fit, s = opt_lambda, newx = K1_)
lines(x,f2,col='green',lwd=2)
m=200
Omega = cbind(rnorm(m)) # Squared exponential kernel
Proj = scale(x) %*% t(Omega) # Projection - combine data with sample frequencies
Phi = sqrt(1/m) * cbind(cos(Proj/alpha), sin(Proj/alpha)) # Fourier feature matrix
lambdas <- 10^seq(5, -5, length.out=100)
cv_fit <- cv.glmnet(Phi, z_, alpha = 0, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min
fit <- glmnet(Phi, z_, alpha = 0, lambda = opt_lambda)
f2 <- predict(fit, s = opt_lambda, newx = Phi)
lines(x,f2,col='brown',lwd=6)
