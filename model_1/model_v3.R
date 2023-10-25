library(PBSddesolve)
library(deSolve)
source("./patientClass.R")
source("./model_1/modelUtils.R")
source("./data/params.R")
source("./model_1/initialConditions.R")
source("./data/patientsData.R")

Patient <- P1

# Données initiales pour lagvalues
a <- get_initial_conditions(Patient)
C_0 <- get_C(a$y0_0,a$y1_0,a$y2_0,a$y3_0,a$z0_0,a$z1_0,a$z2_0,a$z3_0)
p_CT_0 <- get_p_CT(0,Patient,C_0,a$T_0,parameters$k)

print(paste("C_0:",C_0))
print(paste("p_CT_0:",p_CT_0))

equations_dede <- function(t, state, parameters, ...) {
  with(as.list(c(state, parameters)), {
    
    # Extraction des variables d'état à t
    y0 <- state["y0"]
    y1 <- state["y1"]
    y2 <- state["y2"]
    y3 <- state["y3"]
    z0 <- state["z0"]
    z1 <- state["z1"]
    z2 <- state["z2"]
    z3 <- state["z3"]
    T <- state["T"]
    
    # Extraction des variables d'état à t-n*tau
    y0_n_tau <- ifelse(t<Patient$n*tau,a$y_0_0,lagvalue(t - Patient$n*tau, 1))
    y1_n_tau <- ifelse(t<Patient$n*tau,a$y_1_0,lagvalue(t - Patient$n*tau, 2))
    y2_n_tau <- ifelse(t<Patient$n*tau,a$y_2_0,lagvalue(t - Patient$n*tau, 3))
    y3_n_tau <- ifelse(t<Patient$n*tau,a$y_3_0,lagvalue(t - Patient$n*tau, 4))
    z0_n_tau <- ifelse(t<Patient$n*tau,a$z_0_0,lagvalue(t - Patient$n*tau, 5))
    z1_n_tau <- ifelse(t<Patient$n*tau,a$z_1_0,lagvalue(t - Patient$n*tau, 6))
    z2_n_tau <- ifelse(t<Patient$n*tau,a$z_2_0,lagvalue(t - Patient$n*tau, 7))
    z3_n_tau <- ifelse(t<Patient$n*tau,a$z_3_0,lagvalue(t - Patient$n*tau, 8))
    T_n_tau <- ifelse(t<Patient$n*tau,a$T_0,lagvalue(t - Patient$n*tau, 9))
    
    # Calculs de C et p_CT
    C <- get_C(y0,y1,y2,y3,z0,z1,z2,z3)
    p_CT <- get_p_CT(t,Patient,C,T,parameters$k)

    # Calcul des lagvalues
    C_n_tau <- get_C(y0_n_tau,y1_n_tau,y2_n_tau,y3_n_tau,z0_n_tau,z1_n_tau,z2_n_tau,z3_n_tau)
    p_CT_n_tau <- get_p_CT(t-Patient$n*tau,Patient,C_n_tau,T_n_tau,parameters$k)
    
    # Dérivées x9
    dy0 <- (r_y * (1 - u) - d0) * y0 - q_c * p_CT * y0
    dy1 <- parameters$a_y * y0 - d1 * y1 - q_c * p_CT * y1
    dy2 <- parameters$b_y * y1 - parameters$d2 * y2 - parameters$q_c * p_CT * y2
    dy3 <- parameters$c_y * y2 - parameters$d3 * y3 - parameters$q_c * p_CT * y3
    dz0 <- (r_z - d0) * z0 + r_y * y0 * u - parameters$q_c * p_CT * z0
    dz1 <- parameters$a_z * z0 - parameters$d1 * z1 - parameters$q_c * p_CT * z1
    dz2 <- parameters$b_z * z1 - parameters$d2 * z2 - parameters$q_c * p_CT * z2
    dz3 <- parameters$c_z * z2 - parameters$d3 * z3 - parameters$q_c * p_CT * z3
    dT <-  Patient$s_t-Patient$d_t*T - p_CT*C + (2^Patient$n)*p_CT_n_tau*parameters$q_t*C_n_tau
    
    return(list(c(dy0, dy1, dy2, dy3, dz0, dz1, dz2, dz3, dT))) # x9
  })
}

model_dede <- function(Patient) {
  # Initial conditions, times & lagvalues
  a <- get_initial_conditions(Patient)
  initial_conditions <- c(y0 = a$y0_0, y1 = a$y1_0, y2 = a$y2_0, y3 = a$y3_0, z0 = a$z0_0, z1 = a$z1_0, z2 = a$z2_0, z3 = a$z3_0, T = a$T_0)
  times <- seq(0, months_range*31, by = 1)
  out <- dede(y = initial_conditions, times = times, func = equations_dede, parms = parameters)
  head(out)
  #return(out)
}
print("Lancement")
model_dede(P1)