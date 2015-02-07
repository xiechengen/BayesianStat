Latitude <- c(36.077916, 36.078032, 36.078129, 36.078048, 36.077942, 36.089612, 36.077789, 36.077563)

N = 10^6
MC <- rep(0,N)
IS <- rep(0,N)
xseq <- seq(1,N)
  theta <- rcauchy(1,36.07,0.02)
  thetaq <- rcauchy(1,median(Latitude),10^-4)
  MC[1] <- prod(dcauchy(Latitude,theta,0.0002))
  IS[1] <- prod(dcauchy(Latitude,thetaq,0.0002))*dcauchy(thetaq,36.07,0.02)/dcauchy(thetaq,median(Latitude),10^-4)
for (i in 2:N){
  #generate random theta from prior Cauchy distribution
  theta <- rcauchy(1,36.07,0.02)
  thetaq <- rcauchy(1,median(Latitude),10^-4)
  #Calculate likelihood for Latitude
  MC[i] <- (MC[i-1]*(i-1) + prod(dcauchy(Latitude,theta,0.0002)))/i
  IS[i] <- (IS[i-1]*(i-1) + prod(dcauchy(Latitude,thetaq,0.0002))*dcauchy(thetaq,36.07,0.02)/dcauchy(thetaq,median(Latitude),10^-4))/i
  print(i/N*100)
}

df1 <- data.frame(log(xseq,10),MC)
df1$method <- "Monte Carlo"
df2 <- data.frame(log(xseq,10),IS)
df2$method <- "Importance Sampling"

names(df1) <- c("N","Approx","Method")
names(df2) <- c("N","Approx","Method")


df3 <- rbind(df1,df2)

ggplot(df3, aes(N, Approx, colour = Method)) + geom_line()


plot(log(xseq,10),MC,type = 'l')
lines(log(xseq,10),ISS,type = 'l',col='blue')

