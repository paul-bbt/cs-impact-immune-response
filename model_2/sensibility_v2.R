source("./model_2/program_v4.R")
source("./data/patientsData.R")
parameters = list(
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

# Création de la bdd pour la sensibilité.
Nrep=2
X1<-data.frame(lambda=runif(Nrep, 0.5, 1),k=runif(Nrep, 0.9, 1.1))
X2<-data.frame(lambda=runif(Nrep, 0.5, 1),k=runif(Nrep, 0.9, 1.1))

simulation <- function(studiedParams){
  #matr
  out <-NULL
  for (i in 1:nrow(studiedParams)){
    print(paste(round(100*i/nrow(studiedParams)),"%"))
    # On fait varier 3 paramètres et on garde les autres.
    params <- parameters
    params$lambda <-studiedParams$lambda[i]
    params$k <-studiedParams$k[i]
    out <- modelPierreV4(P1,params)
  }
  return(out)
}

#ressob<-sobol2002(simulation, X1, X2, nboot=10)
#plot(ressob)
print("Launching model")
modelPierreV4(P1,parameters)