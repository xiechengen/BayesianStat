x <- 2

N = 10^6
lambda <- 1
lambda0 <- 1/(10^2)
MC<- rep(0,N)


M <- (lambda0*0+lambda*2) / (lambda0+1)
L <- lambda0+1
theta <- rnorm(N,M ,sqrt(L^-1))

MC<- rep(0,N)
MC[1] <- 1/dnorm(x,theta[i],sqrt(lambda^-1))
for (i in 2:N){
  
  
  MC[i] <- (MC[i-1]*(i-1)+1/dnorm(x,theta[i],sqrt(lambda^-1)))/i
  
  #print(i/N*100)
}


xseq <- seq(1:N)
qplot(log(xseq,10),1/MC,geom='line')



