data <- rep(0,10^4)
data [1] <- rcauchy(i,0,1)
for (i in 2:10^4){
  
    data[i] <- (data[i-1]*(i-1) + rcauchy(i,0,1))/i
    
  print(i/N)
}

x <- seq(1:10^4)
df <- data.frame(x,data)

p <- ggplot(df, aes(x, data))
p + geom_point() + ylim(-100, 100)




