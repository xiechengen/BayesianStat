N <- 500000
y<- c(2, 1, 9, 4, 3, 3, 7, 7, 5, 7)


rGen <- function(n){
  Gen <- rep(0,n)
  x <- runif(n,0,1)
  for(i in 1:n){
    if(x[i]<=0.07){
      Gen[i]<-runif(1,3,4)
    }
    
    else if(x[i]>0.07 & x[i] <= 0.52){
      Gen[i]<-runif(1,4,5)
    }
    
    else if(x[i]>0.52 & x[i]<=0.91){
      Gen[i]<-runif(1,5,6) 
    }
    else{
      Gen[i]<-runif(1,6,7)
    }
  }
  return(Gen)
}

pTheta <- function(theta){
  if(theta>3 & theta<=4){
    p <- 0.07
  }
  else if(theta>4 & theta<=5){
    p <- 0.45
  }
  else if(theta >5 & theta <=6){
    p <- 0.39
  }
  else if(theta >6 & theta <=7){
    p <- 0.09
  }
  else {
    p <- 0 
  
}

return(p)
}

x<-seq(0,10,0.01)
plot(dgamma(x,50,1/0.1))

Theta <- rep(0,1,length(x))
for (i in 1 : length(x)){
  Theta[i] <- pTheta(x[i])
}

plot(x,dgamma(x,50,1/0.1),type='l')
lines(x,Theta,type='l',col='red')




#Posterior
prior2=rep(0,length(x))
prior2[x>3]=0.07
prior2[x>4]=0.45
prior2[x>5]=0.39
prior2[x>6]=0.09
prior2[x>7]=0

pos1 <- dgamma(x,sum(y)+50,length(y)+1/0.1)
plot(x,pos1)

c <- gamma(sum(y)+1)/(length(y)^(sum(y)+1)) * (0.07*(pgamma(4,sum(y)+1,length(y))-pgamma(3,sum(y)+1,length(y)))
                                               +0.45*(pgamma(5,sum(y)+1,length(y))-pgamma(4,sum(y)+1,length(y)))
                                               +0.39*(pgamma(6,sum(y)+1,length(y))-pgamma(5,sum(y)+1,length(y)))
                                               +0.09*(pgamma(7,sum(y)+1,length(y))-pgamma(6,sum(y)+1,length(y))))
  


myfunction<-function(t){
  t^sum(y)*exp(-length(y)*t)*prior2/c
}

pos2=myfunction(x)
plot(x,pos2,ylim=c(0,1))
lines(x,pos1)

post=rgamma(x,shape=98,rate=20)
quantile(post, probs=c(0.025, 0.975))



  
  








