library(deSolve)
library(sensitivity)

#rm(list=ls()) # to clean up memory

n <- 1000
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n))

# Sensitivity analysis
x <- sobol2002(model = sobol.fun, X1, X2, nboot = 100)
print(x)
plot(x)