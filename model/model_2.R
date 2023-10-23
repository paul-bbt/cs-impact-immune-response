library(deSolve)
library(PBSddesolve)
source("./model/modelUtils.R")
source("./params/params.R")
source("./model/initialConditions.R")
source("./params/patientsData.R")

# 1. Check all the params & the functions
# 2. 

Patient <- P1

equations <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Extraction des variables d'état
    y0 <- state["y0"]
    y1 <- state["y1"]
    y2 <- state["y2"]
    y3 <- state["y3"]
    z0 <- state["z0"]
    z1 <- state["z1"]
    z2 <- state["z2"]
    z3 <- state["z3"]
    T <- state["T"]
    
    # Calculs intermédiaires
    C <- get_C(t,y0,y1,y2,y3,z0,z1,z2,z3)
    p_CT <- get_p_CT(t,Patient,C,T,k)
    
    # Lag values
    C_n_tau <- get_C_n_tau(t,y0,y1,y2,y3,z0,z1,z2,z3,n,tau)
    p_CT_n_tau <- get_p_CT_n_tau(t,Patient,C,T,k,n,tau)
  
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
  
model1 <- function(Patient){
  # Initial conditions x9 times
  a <- get_initial_conditions(Patient)
  initial_conditions <- c(y0 = a$y0_0, y1 = a$y1_0, y2 = a$y2_0, y3 = a$y3_0, z0 = a$z0_0, z1 = a$z1_0, z2 = a$z2_0, z3 = a$z3_0, T = a$T_0)
  times <- seq(0, months_range*31, by = 1)
  # Solution
  out <- ode(y = initial_conditions, times = times, func = equations, parms = parameters)
  # utiliser dede et lagvalues
  # Displaying results
  head(out)
  return(out)
}

#print("Début")
#print(P12)
#model(P12)
#Print("Done")
