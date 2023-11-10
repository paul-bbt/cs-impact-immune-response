source("./model_2/program_v4.R")
source("./data/patientsData.R")
par(mfrow = c(1, 2))
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

# On faut varier kambda en observant l'erreur RSME
Patient <- P1
Patient_x <- Patient$time_echelle
Patient_y <- Patient$sfcs_well_echelle

# Variations de lambda et observation du score
lambda_values <- seq(0.55, 0.8, by = 0.001)
rmse_values <- numeric(length(lambda_values))

# Valeur de référence avec lambda du sujet
c1 <- modelPierreV4(Patient, parameters1)
# Res format: time;Lsn;Lpn;Ldn;Lten;Lsr;Lpr;Ldr;Lter;T;V
plot(c1[,10], type = "l", xlab = "time [m]", ylab = "T cells [mol.L-1]", main= Patient$name ,col = "red")
points(Patient$time_echelle, Patient$sfcs_well_echelle)

for (lambda_index in 1:length(lambda_values)) {
  print(paste(round(100*lambda_index/length(lambda_values)),"%"))
  
  new_lambda <- lambda_values[lambda_index]
  parameters2 <- parameters1
  parameters2$lambda <- new_lambda
  parameters2$dS <- 0.003 * new_lambda
  parameters2$dP <- 0.008 * new_lambda
  parameters2$dD <- 0.05 * new_lambda
  parameters2$dTe <- new_lambda
  
  c2 <- modelPierreV4(Patient, parameters2)
  lines(c2[,10], col = "blue")
  
  # Alignement des prédictions du modèle avec les données du patient
  y_model <- c2[,10]
  x_model <- c2[,1]
  aligned_predictions <- numeric(length(Patient_x))
  for (data_index in 1:length(Patient_x)) {
    time_index <- which(x_model == Patient_x[data_index])
    if(length(time_index) == 1) {
      aligned_predictions[data_index] <- y_model[time_index]
    }
  }
  
  # Calcul du RMSE
  errors <- Patient_y - aligned_predictions
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  rmse_values[lambda_index] <- rmse
}

# Affichage des résultats RMSE
print("Plotting RMSE values")
plot(lambda_values, rmse_values, type = "l", xlab = "lambda", ylab = "RMSE", main = "Estimation paramétrique pour lambda")