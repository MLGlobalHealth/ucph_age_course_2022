set.seed(1234)
n=500
x=as.matrix(seq(-2,2,length.out=n))
x=scale(x) # remember to always scale
k=200
# change this (the spectral density)
omega1<-t(cbind(c(rep(3,k)))) 
omega2<-t(rnorm(k,0,5))
omega3<-t(rcauchy(k,0,2))
par(mfrow=c(3,2))
nsamp<-10
for(i in 1:3){
	eval(parse(text=paste0("omega<-omega",i)))
	xxprojected <- x%*%(omega) # project
	f<- sqrt(1/k)*cbind(cos(xxprojected),sin(xxprojected)) # monte carlo bit
	K<-(f)%*%t(f) # recreate kernel matrix
	cK=chol(K+diag(1e-6,n))
	samp<- matrix(rnorm(n*nsamp),nrow=nsamp,ncol=n) %*% cK
	samp<-t(samp)
	hist(omega,20,col='black',main="Spectral Density")
	plot(x,samp[,1],type='l',ylim=c(min(samp),max(samp)),main="Samples")
	for(j in 1:nsamp){
		lines(x,samp[,j],type='l',col=j)
	}
}