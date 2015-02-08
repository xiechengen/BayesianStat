N <- 10^6
data <- rep(0,N)
data [1] <- rcauchy(1,0,1)
for (i in 2:N){
  
    data[i] <- (data[i-1]*(i-1) + rcauchy(1,0,1))/i
    
  print(i/N*100)
}

x <- seq(1:N)
df <- data.frame(x,data)

p <- ggplot(df, aes(log(x,10), data))
p + geom_line() 




