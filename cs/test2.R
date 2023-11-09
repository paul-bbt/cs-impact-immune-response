library(sensitivity)

# MODV: script pour le TD-TP sur analyse d'incertitudes et de sensibilité

#rm(list=ls())
#getwd()
#setwd(getwd())
#tab = read.table("cs/ymax.txt")
#ym = tab$ymax
#hist(ym, breaks=4)
#hist(ym, breaks=3, freq=FALSE)
#hist(ym, breaks=99, freq=FALSE)
#hist(ym, freq=F) 
#?density
#lines(density(ym), col="orange", lwd=3)
#m_estim = mean(ym)
#sd_estim = sd(ym)
#shapiro.test(ym)
#curve(dnorm(x, mean=m_estim, sd= sd_estim), add=TRUE, col="red", lwd=3)

simulation <- function(param){
  out <-NULL
  for (i in 1:nrow(param)){
    out<-c(out, param$y0[i]+10*param$w[i]*param$a[i])
  }
  return(out)
}

Nrep=10000
X1<-data.frame(y0=runif(Nrep, 0, 1), a=rnorm(Nrep, 0.025, 0.005), w=rnorm(Nrep, 40, 5))
X2<-data.frame(y0=runif(Nrep, 0, 1), a=rnorm(Nrep, 0.025, 0.005), w=rnorm(Nrep, 40, 5))
print(X1)
ressob<-sobol2002(simulation, X1, X2, nboot=10)
plot(ressob)
summary(ressob)
print(ressob)