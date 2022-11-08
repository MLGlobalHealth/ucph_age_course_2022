set.seed(123)
n=50
x=seq(0,1,length.out=n)
alpha=0.1
k1=function(x,alpha) exp(-x/alpha)
K1=k1(as.matrix(dist(x)),alpha)
K1_ = t(chol(K1 + diag(1e-8,n)))
gamma = cbind(rnorm(n))
z=K1_%*%gamma 
z_= z+ rnorm(n,0,1)
f=lm.fit(x=K1_,y=z_)
plot(x,z_,pch=16)
lines(x,f$fitted.values,col='red')
lines(x,z,col='blue')
library(glmnet)
lambdas <- 10^seq(5, -5, length.out=100)
cv_fit <- cv.glmnet(K1_, z_, alpha = 0, lambda = lambdas)
opt_lambda <- cv_fit$lambda.min
fit <- glmnet(K1_, z_, alpha = 0, lambda = opt_lambda)
f2 <- predict(fit, s = opt_lambda, newx = K1_)
lines(x,f2,col='green',lwd=2)

library(rstanarm)
prior <- normal( location=0, scale=0.1)
post1 <- stan_glm(as.formula(paste0('y ~ -1+',paste0('x.',1:nrow(K1_),collapse="+"))), data = data.frame(y=z_,x=K1_), 
                  family = gaussian(link = "identity"),iter = 5000,chains=1,prior = prior,thin=10)
        
lines(x, post1$fitted.values,col='brown',lwd=2)
                  
draws=as.matrix(post1)                 
preds = matrix(nrow=nrow(draws),ncol=n)
for(i in 1:nrow(draws)){
	preds[i,] =as.vector(K1_%*%cbind(draws[i,1:n]))
	lines(x, preds[i,],col=rgb(165,42,42,alpha=10,maxColorValue=255),lwd=2)
}
