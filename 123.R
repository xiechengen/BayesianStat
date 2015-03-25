library(mvtnorm)
library(MCMCpack)
#7.3#
Blue = read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/bluecrab.dat")
Org = read.table("http://www.stat.washington.edu/hoff/Book/Data/hwdata/orangecrab.dat")

#bluecrab#
V1 <- Blue[,1]
V2 <- Blue[,2]
m1 <- mean(V1)
m2 <- mean(V2)
mu0<-c(m1,m2)
L0<- matrix(c(var(V1),cov(V1,V2),cov(V1,V2),var(V2)),nrow=2,ncol=2)
nu0<-4
S0<-matrix(c(var(V1),cov(V1,V2),cov(V1,V2),var(V2)),nrow=2,ncol=2)


n <- dim(Blue)[1]
ybar <- apply(Blue,2,mean)
Sigma <- cov(Blue)
THETA <- NULL
SIGMA <- NULL
BlueCor <- NULL

N <- 10000
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
  Sn <- S0 + (t(Blue) - c(theta))%*% t(t(Blue) - c(theta))
  sigma <- solve(rwish(nu0+n,solve(Sn)))
  
  
  ###save results
  THETA <- rbind(THETA,theta)
  SIGMA <- rbind(SIGMA,c(sigma))
  
  ss1[i] <- sigma[1,1]
  ss2[i] <- sigma[2,1]
  ss3[i] <- sigma[1,2]
  ss4[i] <- sigma[2,2]
  ##correlation
  BlueCor=rbind(BlueCor,sigma[1,2]/sqrt(sigma[1,1]*sigma[2,2]))
}

plot(THETA)

plot(density(BlueCor))
plot(density(ss1))
plot(density(ss2))
plot(density(ss3))
plot(density(ss4))

###orange crab
V1 <- Org[,1]
V2 <- Org[,2]
m1 <- mean(V1)
m2 <- mean(V2)
mu0<-c(m1,m2)
L0<- matrix(c(var(V1),cov(V1,V2),cov(V1,V2),var(V2)),nrow=2,ncol=2)
nu0<-4
S0<-matrix(c(var(V1),cov(V1,V2),cov(V1,V2),var(V2)),nrow=2,ncol=2)


n <- nrow(Org)
ybar <- apply(Org,2,mean)
Sigma <- cov(Org)
THETA <- NULL
SIGMA <- NULL
OrgCor <- NULL


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
  Sn <- S0 + (t(Org) - c(theta))%*% t(t(Org) - c(theta))
  sigma <- solve(rwish(nu0+n,solve(Sn)))
  
  ss1[i] <- sigma[1,1]
  ss2[i] <- sigma[2,1]
  ss3[i] <- sigma[1,2]
  ss4[i] <- sigma[2,2]
  
  
  ###save results
  THETA <- rbind(THETA,theta)
  SIGMA <- rbind(SIGMA,c(sigma))
  OrgCor=rbind(OrgCor,sigma[1,2]/sqrt(sigma[1,1]*sigma[2,2]))
}

plot(THETA)
plot(density(OrgCor))

#probability#
as.numeric(summary(c(BlueCor) < c(OrgCor))[3])/10000

