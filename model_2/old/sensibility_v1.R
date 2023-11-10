library(deSolve)
library(sensitivity)
source("./model_2/program_v2.R",)
source("./data/patientsData.R")
dev.off()
print(P1)

simulation <- function(param){
  matr
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