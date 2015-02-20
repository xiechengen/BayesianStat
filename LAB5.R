Sample <- 10000

N<-rep(0,Sample)
Beta<-rep(0,Sample)
N[1] <- 50
Beta[1] <- 0.05

for (i in 2:Sample){
  N[i] <- rpois(1,25*(1-Beta[i-1])) + 20
  Beta[i] <- rbeta(1,21,N[i]-20+1)
}

xseq <- seq(1,10)
plot(xseq,Beta[1:10])
lines(xseq,Beta[1:10])
plot(xseq,N[1:10])
lines(xseq,N[1:10])

quantile(Beta[-c(1:1000)],c(0.05,0.95))

sum(N==20)/Sample



