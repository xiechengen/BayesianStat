N <- 10^3

beta <- rep(0,N)
c <- rep(0,N)
z <- rep(0,N)
x <- divorce[1]
y <- divorce[2]
tou <- 16 
z <- beta * 

GS <- function(){
  y[1] <- 0.9
  x[1] <- 0.85
  
  for (i in 2:N){
    beta[i] <- rnorm(1,(x*z)/(sum(x^2)+1/tou),1/(sum(x^2)+1/tou)
    
    ## generating z
    if(y == 1){
      zg <- rnorm(1,beta*x,1)
      while(zg > c){
        z <- zg
      }
    }
    else{
      while(zg < c)
      z <- rnorm(1,beta*x,1)
    }
    
    
    #generating c
    while (c>cmin & c<cmax){
      
    }
    
    
    
  }
  
  
  
  
  
  for(i in 1:N){
    c[i] <- rtruncnorm(1,max(z[i,(y==0)]),min(z[i,(y==1)]),0,4)
    b[i] <- rnorm(1,(16*t(x)%*%z[i,]/(1+16*x2)),sqrt(16/(1+16*x2)))
    for(j in 1:25){
      if(y[j]==0){
        z[i+1,j] <- rtruncnorm(1,-Inf,c[i],b[i]*x[j],1)
      }else{
        z[i+1,j] <- rtruncnorm(1,c[i],Inf,b[i]*x[j],1)
      }
    }
  }

  
  
  
  
