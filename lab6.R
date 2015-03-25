mean <- c(0,0,0)
cor <- matrix(c(1,0.9,0.1,0.9,1,0.1,0.1,0.1,1),nrow=3,ncol=3)
N <- 1000

x <- rep(0,1,N)
y <- rep(0,1,N)
z <- rep(0,1,N)

for(i in 2:N){
  x[i] <- rnorm(1,0.899*y[i-1] + 0.0101*z[i-1],0.1899)
  y[i] <- rnorm(1,0.899*x[i-1] + 0.0101*z[i-1],.1899)
  z[i] <- rnorm(1,0.0526*x[i-1] + 0.0526*y[i-1],0.9895)
}

xseq <- seq(1:N)
plot(xseq,x,type='l')


x <- rep(0,1,N)
y <- rep(0,1,N)
z <- rep(0,1,N)


for(i in 2:N){
  a <- rmvnorm(1, c(0.1,0.1)*z[i-1],matrix(c(0.99,0.89,0.89,0.99),nrow=2, ncol=2))
  x[i] <- a[1]
  y[i] <- a[2]
  z[i] <- rnorm(1,0.0526*x[i-1] + 0.0526*y[i-1],0.9895)  
}

xseq <- seq(1:N)
plot(xseq,x,type='l')
