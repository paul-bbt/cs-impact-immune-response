library(deSolve)
source("./modelUtils.R")
source("./params/params.R")
source("./initialConditions.R")
source("./params/patientsData.R")
#source("./model.R")

Patient <- P12

equations <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    print("Test")
    print(names(state))
    
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
    
    # Parmètres qui dépendent du temps
    d0 <- get_d0(t)
    r_z <- get_r_z(t)
    u <- get_u(t)
    K <- get_K(t)
    tau <- get_tau(t)
    r_y <- get_r_y(t)
    
    # Calculs intermédiaires
    C <- get_C(t,y0,y1,y2,y3,z0,z1,z2,z3)
    C_n_tau <- get_C_n_tau(t,y0,y1,y2,y3,z0,z1,z2,z3,n,tau)
    p <- get_p_CT(t,Patient,C,T,k)
    p_CT_n_tau <- get_p_CT_n_tau(t,Patient,C,T,k,n,tau)
  
    # Dérivées
    dy0 <- (r_y * (1 - u) - d0) * y0 - q_c * p_CT * y0
    dy1 <- ay * y0 - d1 * y1 - q_c * p_CT * y1
    dy2 <- params$by * y1 - params$d2 * y2 - params$q_c * p_CT * y2
    dy3 <- params$cy * y2 - params$d3 * y3 - params$q_c * p_CT * y3
    dz0 <- (r_z(t) - d0(t)) * z0 + r_y(t) * y0 * u(t) - params$q_c * p_CT * z0
    dz1 <- params$az * z0 - params$d1 * z1 - params$q_c * p_CT * z1
    dz2 <- params$b_z * z1 - params$d2 * z2 - params$q_c * p_CT * z2
    dz3 <- params$c_z * z2 - params$d3 * z3 - params$q_c * p_CT * z3
    dT <-  Patient$s_T-Patient$d_T*T - p*C + (2^Patient$n)*p_CT*params$q_t*C_n_tau
    
    return(list(c(dy0, dy1, dy2, dy3, dz0, dz1, dz2, dz3, dT)))
  })
}

# Initial conditions, times
a <- get_initial_conditions(Patient)
months_range <- 50
initial_conditions <- c(y0 = a$y0_0, y1 = a$y1_0, y2 = a$y2_0, y3 = a$y3_0, z0 = a$z0_0, z1 = a$z1_0, z2 = a$z2_0, z3 = a$z3_0, T = a$T_0)
times <- seq(0, months_range*31, by = 1)

# Solution
out <- ode(y = initial_conditions, times = times, func = equations, parms = parameters)

# Displaying results
head(out)
