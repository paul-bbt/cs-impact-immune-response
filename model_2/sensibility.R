library(deSolve)
library(sensitivity)
setwd(getwd())
source("./model_2/program_v2.R",)
source("./data/patientsData.R")

print(P1)

 # to clean up memory
# Documentation https://www.rdocumentation.org/packages/sensitivity/versions/1.7/topics/sobol2002

model_function <- function(parameters) {
  res <- modelPierre(P1)
  # Res format: time;Lsn;Lpn;Ldn;Lten;Lsr;Lpr;Ldr;Lter;T;V
  return(res[, "T"])
}
parameters <- c(
  "c_n", "y0_0", "an1", "bn1", "cn1", "an0", "bn0", "cn0",
  "ar", "br", "cr", "qC", "p0", "k", "tau"
)
n_samples <- 500
n_parameters <- length(parameters)
total_samples <- n_samples * 10
X1 <- data.frame(matrix(runif(n_parameters * total_samples), nrow = total_samples))
X2 <- data.frame(matrix(runif(n_parameters * total_samples), nrow = total_samples))
X1 <- X1[1:n_samples,]
X2 <- X2[1:n_samples,]
colnames(X1) <- parameters
colnames(X2) <- parameters
sensitivity_indices <- sobol2002(model = model_function, X1, X2, nboot = 100)
print(sensitivity_indices)