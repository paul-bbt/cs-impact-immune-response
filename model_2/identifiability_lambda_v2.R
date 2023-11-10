library(ggplot2)
source("./model_2/program_v4.R")
source("./data/patientsData.R")

parameters1 = list(
  lambda = 0.75, # Parametre d'ajustement
  dS = 0.003 * lambda, # Death rate Ls
  dP = 0.008 * lambda, # Death rate Lp
  dD = 0.05 * lambda, # Death rate Ld
  dTe = lambda, # Death rate Lte
  rn = 0.008, # Growth rate for non-resistant cells (to imatinib)
  rr = 0.0023, # Growth rate for resistant cells
  an0 = 1.6, # Transfer rate. 
  an1 = an0 / 100,
  ar0 = an0,
  ar1 = ar0,
  ar = an0,
  bn0 = 10, 
  bn1 = bn0 / 750,
  br0 = bn0,
  br1 = br0,
  br = bn0,
  cn0 = 100, 
  cn1 = cn0,
  cr0 = cn0,
  cr1 = cr0,
  cr = cn0,
  u = 4 * 10^(-8), # Mutation rate from non-resistant to resistant
  ur = 0, # Mutation rate from resistant to non-resistant
  p0 = 0.8, # Prob T engages a cell cancer
  qC = 0.75, # Prob cancer cell dies from encounter
  qT = 0.5, # Prob T survives an encounter
  k = 1, # Kinetic coefficient
  tau = 1 # Duration of one T cell division (day)
)

Patient <- P1
# Variations de lambda
lambda_values <- seq(0.5, 1, by = 0.1)
rmse_values <- numeric(length(lambda_values))

# Valeur de référence
c1 <- modelPierreV4(Patient, parameters1)
plot(c1[,10], type = "l", xlab = "Temps [m]", ylab = "T cells [mol.L-1]", main= Patient$name ,col = "red")
points(Patient$time_echelle, Patient$sfcs_well_echelle)

for (i in 1:length(lambda_values)) {
  print(paste(round(100*i/length(lambda_values)),"%"))
  
  new_lambda <- lambda_values[i]
  parameters2 <- parameters1
  parameters2$lambda <- new_lambda
  parameters2$dS <- 0.003 * new_lambda
  parameters2$dP <- 0.008 * new_lambda
  parameters2$dD <- 0.05 * new_lambda
  parameters2$dTe <- new_lambda
  
  c2 <- modelPierreV4(Patient, parameters2)
  lines(c2[, 10], col = "blue")
}

