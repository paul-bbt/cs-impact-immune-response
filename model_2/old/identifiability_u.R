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

# Identifiability
# Variations on lambda
u_values <- seq(1 * 10^(-8), 8 * 10^(-8), by = 1 * 10^(-8))
print(length(u_values))
rmse_values <- numeric(length(u_values))
c1 <- modelPierreV4(P1, parameters1)

for (i in 1:length(u_values)) {
  print(paste(round(100*i/length(u_values)),"%"))
  
  new_u <- u_values[i]
  parameters2 <- parameters1
  parameters2$u <- new_u
  
  c2 <- modelPierreV4(P1, parameters2)[,10]
  
  # Calculate RMSE
  squared_diff <- (c1 - c2)^2
  mean_squared_diff <- mean(squared_diff)
  rmse <- sqrt(mean_squared_diff)
  
  rmse_values[i] <- rmse
}
print("Plotting values")
plot(u_values, rmse_values, type = "l", xlab = "p0", ylab = "RMSE", main = "RMSE vs. p0")