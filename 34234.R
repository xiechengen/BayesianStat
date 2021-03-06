library(MCMCpack)
Y = as.matrix(na.omit(hier[,2:11]))
x <- seq(1:10) - 5.5

N <- 3000
m0 <- 12
s0 <- 1
m1 <-1
s1 <- 1
n <- nrow(Y)
a <- 5
l <- 4

mb0 <- matrix(rep(0,73*N),nrow = 73)
mb1 <- matrix(rep(0,73*N),nrow = 73)

mu0 <- rep(0,N)
mu1 <- rep(0,N)
t0 <- rep(0,N)
t1 <- rep(0,N)
t <- rep(0,N)

for (z in 2:N){

b0 <- rep(0,n)
#sampler for beta0 
for(i in 1:n){
  Kb0 <- 0
  for (j in 1:10){
    Kb0<- Kb0 + Y[i,j] - b1[i] * x[j]
  }
  
  t0b <- (r0^-1 + 10*r^-1)^-1
  m0b <- (t0b * (r0^-1 * mu0 + t^-1 * Kb0))

  b0[i] <- rnorm(1,m0b,sqrt(t0b))
}

mb0[z,] <- b0

b1 <- rep(0,n)
#sampler for beta1
for(i in 1:n){
  Kb1 <- 0
  for (j in 1:10){
    Kb1<- Kb1 + x[j](Y[i,j] -b0[i])
  }

  
  t1b <- (t1[z-1]^-1 + t^-1 *sum(x^2))
  m1b <- t1b(t1[z-1]^-1 * mu1[z-1] + t[z-1]^-1 * Kb1)
  b1[i] <- rnorm(1,m1b,sqrt(t1b))
}

mb1[z,] <- b1

  #sampler for mu0
  s0m <- (s0^-1 + n * t0[z-1] ^ -1)^-1
  m0m <- s0m(s0^-1 * m0 + t0[z-1]^-1 * sum(b0))
  
  
  mu0[i] <- rnorm(1,m0m,sqrt(s0m))
  
  #sampler for mu1
  s1m <- (s1^-1 + n * r1^-1)^-1
  m1m <- s1m(s1^-1*m1 + t1[z-1]^-1 * sum(b1))

  mu1[i] <- rnorm(1,m1m,sqrt(s1m))
  


  #sampler t0
  sss <- 0
  for(i in 1:n){
    sss <- sss + (b0[i]-mu0[z-1])^2
  }
  t0[i] <- rinvgamma(1,a+n/2,l + sss/2)
  
  #sampler t1  
  sss <- 0
  for(i in 1:n){
  sss <- sss + (b1[i]-mu1[z-1])^2
  }
  t1[z] <- rinvgamma(1,a+n/2,l + sss/2)
  
  #sampler t
  ckj<- 0;
  for(i in 1:n){
  kj <- 0;
  for(j in 1:10 ){
    kj<- kj+ (Y[i,j] - b0[i] - b1[i]*x[j])^2
  }
  ckj <- ckj + kj
  }
  
  t[z] <- rinvgamma(1,a+n*10/2,l + ckj/2)
}


(mb1 >= 0.5)/N


