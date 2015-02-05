Latitude <- c(36.077916, 36.078032, 36.078129, 36.078048, 36.077942, 36.089612, 36.077789, 36.077563)


N1 = 10000000000
N2 = 100
MC <- rep(0,N1)

for(i in 1:N1){
  #generate random parameter theta from prior distribution Cauchy
  theta <- rcauchy(1,36.07,0.02)
  #generarte random sample and calculate proximation
  MC[i] <- sum(rcauchy(N1, theta, 0.0002))/N1
}

#for(i in 1:N1)

xseq <- seq(0,N1-1)
plot(xseq,MC,type = 'l')