x<- divorce[1]
y<- divorce[2]


x2<- sum(x^2)
N<-20000
z<-matrix(rep(0,(N+1)*25),N+1,25)
r<-runif(1,0,1)
for(k in 1:25){
  if(y[k,]==0){
    z[1,k]<-runif(1,-10,r)
  }else{
    z[1,k]<-runif(1,r,10)
  }
}

c <- rep(0,N)
b <- rep(0,N)
meb <- rep(0,N)
mec <- rep(0,N)
xseq<-seq(1,N,1)

for(i in 1:N){
  c[i] <- rtruncnorm(1,max(z[i,(y==0)]),min(z[i,(y==1)]),0,4)
  b[i] <- rnorm(1,(16*t(x)%*%z[i,]/(1+16*x2)),sqrt(16/(1+16*x2)))
  for(j in 1:25){
    if(y[j]==0){
      z[i+1,j] <- rtruncnorm(1,-Inf,c[i],b[i]*x[j,],1)
    }else{
      z[i+1,j] <- rtruncnorm(1,c[i],Inf,b[i]*x[j,],1)
    }
  }
}



#par(mfrow=c(1,1))
plot(xseq,b,col="green3",main="Beta")
plot(xseq,c,col="red2",main="c")
print(r)
print(mean(b[i>18000]))
print(mean(c[i>18000]))

meb[1] <- b[1]
mec[1] <- c[1]
for(i in 2:N){
  meb[i]<-meb[i-1]*(i-1)/i+b[i]/i
  mec[i]<-mec[i-1]*(i-1)/i+c[i]/i
}

plot(xseq, meb,type= 'l', col = "yellow3",main="running average of Beta")
plot(xseq, mec,type= 'l', col = "blue3",main="running average of c")


par(mfrow=c(3,1))
acf(b)
acf(c)
acf(z[,1])

b2 <-b[-c(0:18000)]
quantile(b2,c(0.025,0.975))
print(sum(b2>0)/2000)
