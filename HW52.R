x <- 2

N = 10^6
lambda <- 1
lambda0 <- 1/(10^2)
MC<- rep(0,N)

M <- (lambda0*0+lambda*2) / (lambda0+1)
L <- lambda0+1
  
  MC<- rep(0,N)
  for (i in 1:N){
   
    theta <- rnorm(1,M ,sqrt(L))
    MC[i] <- 1/dnorm(x,theta,sqrt(lambda^-1))
    
    print(i/N)
  }
 

1/(mean(MC))
dnorm(2,0,sqrt(lambda^-1 + lambda0^-1))




