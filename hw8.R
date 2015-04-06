library(rowr)
library(devtools)
library(LaplacesDemon)
library(MCMCpack)


d1=read.table('http://www.stat.washington.edu/people/pdhoff/Book/Data/hwdata/school1.dat')
d2=read.table('http://www.stat.washington.edu/people/pdhoff/Book/Data/hwdata/school2.dat')
d3=read.table('http://www.stat.washington.edu/people/pdhoff/Book/Data/hwdata/school3.dat')
d4=read.table('http://www.stat.washington.edu/people/pdhoff/Book/Data/hwdata/school4.dat')
d5=read.table('http://www.stat.washington.edu/people/pdhoff/Book/Data/hwdata/school5.dat')
d6=read.table('http://www.stat.washington.edu/people/pdhoff/Book/Data/hwdata/school6.dat')
d7=read.table('http://www.stat.washington.edu/people/pdhoff/Book/Data/hwdata/school7.dat')
d8=read.table('http://www.stat.washington.edu/people/pdhoff/Book/Data/hwdata/school8.dat')


Y <- cbind.fill(d1,d2,d3,d4,d5,d6,d7,d8,fill=NA)


##### MCMC analysis for school data

### weakly informative priors
nu0<-2
s20<-15
eta0<-2
t20<-10
mu0<-7
g20<-5
###

### starting values
m<-length(Y) 
n<-sv<-ybar<-rep(NA,m) 
logic<-as.matrix(!Y)

for (j in 1:m) 
{ 
  n[j]<-length(which(logic[,j]==FALSE))
  ybar[j]<-sum(Y[,j],na.rm=NA)/n[j]
}

theta<-ybar
sigma2<-0.5
mu<-mean(theta)
tau2<-var(theta)

set.seed(1)
S<-5000
THETA<-matrix( nrow=S,ncol=m)
MST<-matrix( nrow=S,ncol=3)

for(s in 1:S) 
{
  
  # sample new values of the thetas
  for(j in 1:m) 
  {
    vtheta<-1/(n[j]/sigma2+1/tau2)
    etheta<-vtheta*(ybar[j]*n[j]/sigma2+mu/tau2)
    theta[j]<-rnorm(1,etheta,sqrt(vtheta))
  }
  
  #sample new value of sigma2
  nun<-nu0+sum(n)
  ss<-nu0*s20
  
  for(j in 1:m){ss<-ss+sum((Y[j]-theta[[j]])^2,na.rm = NA)}
  sigma2<-1/rgamma(1,nun/2,ss/2)
  
  #sample a new value of mu
  vmu<- 1/(m/tau2+1/g20)
  emu<- vmu*(m*mean(theta)/tau2 + mu0/g20)
  mu<-rnorm(1,emu,sqrt(vmu)) 
  
  # sample a new value of tau2
  etam<-eta0+m
  ss<- eta0*t20 + sum( (theta-mu)^2 )
  tau2<-1/rgamma(1,etam/2,ss/2)
  
  #store results
  THETA[s,]<-theta
  MST[s,]<-c(mu,sigma2,tau2)
  MU<-mu
  
} 


mcmc1<-list(THETA=THETA,MST=MST)

plot(MST[1:S,1],type="l",main=" mu")
y_lag <- filter(MST[1:S,1], rep(1/30, 30), sides=1)
lines(y_lag, col="red")

plot(MST[1:S,2],type="l",main="sigma2")
y_lag <- filter(MST[1:S,2], rep(1/30, 30), sides=1)
lines(y_lag, col="red")

plot(MST[1:S,3],type="l",main="tau2")
y_lag <- filter(MST[1:S,2], rep(1/30, 30), sides=1)
lines(y_lag, col="red")


#b)
mean((MST[,1]))
x1<- seq(0,14,length=1000)
p.interval(MST[,1], HPD=FALSE, MM=FALSE, plot=TRUE)
mu0<-dnorm(x1,7,5^1/2)
points(x1,mu0, type="l",col="red",lwd=3)

mean((MST[,2]))
x2 <- seq(0,30,length=1000)
p.interval(MST[,2], HPD=FALSE, MM=FALSE, plot=TRUE)
sigma20<-dinvgamma(x2,1, 15)
points(x2,sigma20, type="l",col="red",lwd=2)
plot(x2,sigma20, type="l",col="red",lwd=2)

mean((MST[,3]))
x3<- seq(0,30,length=1000)
p.interval(MST[,3], HPD=FALSE, MM=FALSE, plot=TRUE)
tau20<-dinvgamma(x3,1, 10)
points(x3,tau20, type="l",col="red",lwd=2)
plot(x3,tau20, type="l",col="green",lwd=3)

# c)
  mu<- rnorm(10000,mu0,sqrt(g20))
  tau<- 1/rgamma(10000,eta0/2,(eta0*t20)/2)
  sigma<- 1/rgamma(10000,nu0/2,(nu0*s20)/2)
  
  R<-rep(0,S)
  R<-(MST[,3]/(MST[,2]+MST[,3]))
  plot(density(R),xlim = c(-0.2,1.5),ylim=c(0,5),col="darkblue")
  R0<-(tau/(sigma+tau))
  lines(density(R0),col="darkred")

#Part d)
ptheta <- mean(theta[7]<theta[6])

p1<- mean(theta[7]<theta[1])
p2<- mean(theta[7]<theta[2])
p3<- mean(theta[7]<theta[3])
p4<- mean(theta[7]<theta[4])
p5<- mean(theta[7]<theta[5])
p6<- mean(theta[7]<theta[6])
p8<- mean(theta[7]<theta[8])
p<- p1*p2*p3*p4*p5*p6*p8

#Part e)
meanTheta <- rep(0,8)

for (i in 1:8){
  meanTheta[i] <- mean(THETA[,i])
}
plot(meanTheta,ybar,type="l")
