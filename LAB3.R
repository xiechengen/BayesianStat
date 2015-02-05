
```{r}

library(ggplot2)
density.function <- function(x){
  return((x^4)* (15 * (exp(-(x/2)^5)))+(5/81)*exp(-(x/6)^5))
}

xseq <- seq(0,10,0.01)
qplot(xseq,density.function(xseq),geom = 'line')





```


abc