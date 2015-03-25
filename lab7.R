library(MCMCpack)
library(mvtnorm)


mydata1 = read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/bluecrab.dat")
mydata2 = read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/orangecrab.dat")

V1 <- mydata1[,1]
V2 <- mydata1[,2]
m1 <- mean(V1)
m2 <- mean(V2)
mu0<-c(m1,m2)
L0<- matrix(c(var(V1),cov(V1,V2),cov(V1,V2),var(V2)),nrow=2,ncol=2)
nu0<-4
S0<-matrix(c(var(V1),cov(V1,V2),cov(V1,V2),var(V2)),nrow=2,ncol=2)


n <- dim(mydata1)[1]
ybar <- apply(mydata1,2,mean)
Sigma <- cov(mydata1)


N <- 10000

THETA <- rep(0,N)
SIGMA <- rep(0,N)
BlueCov <- rep(0,N)

#save data into vector
ss1 <- rep(0,N)
ss2 <- rep(0,N)
ss3 <- rep(0,N)
ss4 <- rep(0,N)

for (i in 1:N){
  ###update theta
  Ln <- solve(solve(L0) + n*solve(Sigma))
  mun <- Ln%*%(solve(L0)%*%mu0 + n*solve(Sigma)%*%ybar)
  theta <- rmvnorm(1,mun,Ln)
  
  
  ###update Sigma
  Sn <- S0 + (t(mydata1) - c(theta))%*% t(t(mydata1) - c(theta))
  sigma <- solve(rwish(nu0+n,solve(Sn)))
  
  ss1[i] <- sigma[1,1]
  ss2[i] <- sigma[2,1]
  ss3[i] <- sigma[1,2]
  ss4[i] <- sigma[2,2]
  
  ###save results
  THETA[i] <- theta
  SIGMA[i] <- c(sigma)
  
  
  ##correlation
  cor_blue=rbind(cor_blue,sigma[1,2]/sqrt(sigma[1,1]*sigma[2,2]))
}

plot(THETA)

plot(density(BLUE))
plot(density(ss1))
plot(density(ss2))
plot(density(ss3))
plot(density(ss4))

