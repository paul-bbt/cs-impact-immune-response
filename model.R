library(deSolve)
source("./Params/params_1.R")

# Main model function
resolve <- function(params){
  months_range <- 50
  
  times <- seq(0, months_range*31, by = 1)
  parameters <- NULL
  state <- NULL
  
  out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
  head(out)
  
  return(0)
}

# Utils functions
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

# To be checked
get_C <- function(day,y0,y1,y2,y3,z0,z1,z2,z3){
  return(sum(c(y0,y1,y2,y3,z0,z1,z2,z3))) 
}

# To be checked
get_C_n_tau <- function(day,y0,y1,y2,y3,z0,z1,z2,z3,n,tau){
  return(get_C(day-n*tau,y0,y1,y2,y3,z0,z1,z2,z3))
}

# To be checked
get_p_CT <- function (day,Patient,C,T,k){
  return(params$p_0*exp(-Patient$c_n*get_C())*k*T)
}

# To be checked
get_p_CT_n_tau <- function(day,Patient,C,T,k,n,tau){
  return(get_p_CT(day-n*tau,Patient,C,T,k))
}

# Equations dt (1)
# Check everything here
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

get_dy0 <- function(day,y0){
  return((r_y(day) * (1 - u(day)) - d0(day)) * y0 - params$q_c * get_p_CT() * y0)
}

get_dy1 <- function(y0, y1, y2){
  return(params$ay * y0 - params$d1 * y1 - params$q_c * get_p_CT() * y1)
}

get_dy2 <- function(y1, y2){
  return(params$by * y1 - params$d2 * y2 - params$q_c * get_p_CT() * y2)
}

get_dy3 <- function(y2, y3){
  return(params$cy * y2 - params$d3 * y3 - params$q_c * get_p_CT() * y3)
}

get_dz0 <- function(day,y0, z0){
  return((r_z(day) - d0(day)) * z0 + r_y(day) * y0 * u(day) - params$q_c * get_p_CT() * z0)
}

get_dz1 <- function(z0, z1){
  return(params$az * z0 - params$d1 * z1 - params$q_c * get_p_CT() * z1)
}

get_dz2 <- function(z1, z2){
  return(params$b_z * z1 - params$d2 * z2 - params$q_c * get_p_CT() * z2)
}

get_dz3 <- function(z2, z3){
  return(params$c_z * z2 - params$d3 * z3 - params$q_c * get_p_CT() * z3)
}

# Equations dt (2)
# Check everything here
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

get_dT <- function(day,T,Patient,k,y_0,z_0){
  A <- Patient$s_T-Patient$d_T*T
  B <- get_p()*get_C()
  C <- (2^Patient$n)*get_p_CT_n_tau()*params$q_t*get_C_n_tau()
  return(A-B+C)
}