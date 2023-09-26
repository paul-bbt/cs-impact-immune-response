source("./Params/params_1.R")

# Main model function
model <- function(params){
  return(0)
}

# Utils functions
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

# Not done
get_p_CT <- function (P,C,T,k){
  return(params$p_0*exp(-P$c_n*C)*k*T)
}

# Not done
get_p_CT_n_tau <- function(){
  return(0)
}

# FAUX ! Revoir, attention à t !
get_C <- function(y0,z0){
  y_1 <- get_y_1(y0)
  y_2 <- get_y_1(y1)
  y_3 <- get_y_1(y2)
  z_1 <- get_y_1(y0)
  z_2 <- get_y_1(y1)
  z_3 <- get_y_1(y2)
  return(sum(c(y0,y1,y2,y3,z0,z1,z2,z3))) 
}

# Not done
get_C_n_tau <- function(){
  return(0)
}

# Equations dt (1)
# Check everything here
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

# WARNING r_y, d_0 & u
get_dy0 <- function(y0) {
  return((r_y * (1 - u) - d0) * y0 - params$q_c * get_p_CT() * y0)
}

get_dy1 <- function(y0, y1, y2) {
  return(params$ay * y0 - params$d1 * y1 - params$q_c * get_p_CT() * y1)
}

get_dy2 <- function(y1, y2) {
  return(params$by * y1 - params$d2 * y2 - params$q_c * get_p_CT() * y2)
}

get_dy3 <- function(y2, y3) {
  return(params$cy * y2 - params$d3 * y3 - params$q_c * get_p_CT() * y3)
}

# WARNING r_z, r_y, d0 & u
get_dz0 <- function(y0, z0) {
  return((rz - d0) * z0 + ry * y0 * u - params$q_c * get_p_CT() * z0)
}

get_dz1 <- function(z0, z1) {
  return(params$az * z0 - params$d1 * z1 - params$q_c * get_p_CT() * z1)
}

get_dz2 <- function(z1, z2) {
  return(params$b_z * z1 - params$d2 * z2 - params$q_c * get_p_CT() * z2)
}

get_dz3 <- function(z2, z3) {
  return(params$c_z * z2 - params$d3 * z3 - params$q_c * get_p_CT() * z3)
}

# Equations dt (2)
# Check everything here
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

get_dT <- function(T,Patient,k,y_0,z_0){
  A <- Patient$s_T-Patient$d_T*T
  B <- get_p()*get_C()
  C <- (2^Patient$n)*get_p_CT_n_tau()*params$q_t*get_C_n_tau()
  return(A-B+C)
}