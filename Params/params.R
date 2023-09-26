lambda <-0.75 # Fractional adjustment constant
d0_seed <- 0.003 # SC Rate WARNING
d1 <- 0.008*lambda # PC death Rate
d2 <- 0.05*lambda # DC death Rate
d3 <- lambda # TC death Rate
r_y_seed <- 0.008 # Growth rate for nonresistant cells WARNING
a_y <- 1.6 # Rates without imatinib treatment
b_y <- 10
c_y <- 100 
a_p_y <- a_y/100 # Rates during imatinib treatment
b_p_y <- b_y/750 
c_p_y <- c_y 
r_z_seed <- 0.023 # Growth rate for resistant cells WARNING
a_z <- a_y # Rates for resistant cells
b_z <- b_y 
c_z <- c_y
u_seed <- 4*e-8 # Mutation rate per division WARNING
K_seed <- 1 # Kinetic coefficient WARNING
p0 <- 0.8 # Prob. T cell engages cancer cell
q_c <- 0.75 # Prob. cancer cell dies from encounter
q_t <- 0.5 # Prob. T cell survives encounter
tau_seed <- 1 # Duration of one T cell division

# Variables et constantes
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# d_T Anti-leukemia T cell death rate see in patient Data
# s_T Anti-leukemia T cell supply rate patient Data
# c_n Decay in  rate of immmune responsivity patient Data

d0 <- function (day){
  return(d_0_seed*lambda/day)
}
r_z <- function (day){
  return(r_z_seed/day)
}
u <- function (division){
  return(u_seed/division)
}
K <- function (KuL,day){
  return((K_seed/Kul)/day)
}

tau <- function (day_unit){
  return(tau_seed*day_unit)
}

r_y <- function(day){
  return(r_y_seed/day)
}

#-------------------------------------------------------------------
#-------------------------------------------------------------------

get_y1 <- function (y_0){
  return(a_y*y_0/d_1)
}

get_y2 <- function (y_1){
  return (b_y*y_1/d_2)
}

get_y3 <- function (y_2){
  return (c_y*y_2/d_3)
}

get_z1 <- function(z_0){
  return (a_z*z_0/d1)
}

get_z2 <- function(z_1){
  return (b_z*z_1/d2)
}

get_z3 <- function(z_2){
  return (c_z*z_2/d3)
}

get_Y0 <- function(s_T,d_T){
  return(s_T/d_T)
}

params <- list(
  lambda = lambda,
  #d0 = NULL,
  d1 = d1,
  d2 = d2,
  d3 = d3,
  r_y = r_y,
  a_y = a_y,
  b_y = b_y,
  c_y = c_y,
  a_p_y = a_p_y,
  b_p_y = b_p_y,
  c_p_y = c_p_y,
  #r_z = NULL,
  a_z = a_z,
  b_z = b_z,
  c_z = c_z,
  #u = NULL,
  #K = NULL,
  p0 = p0,
  q_c = q_c,
  q_t = q_t,
  #tau = tau
)