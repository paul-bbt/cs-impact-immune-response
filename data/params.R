# Les paramètres qui dépendent du temps doivent être dans la page model
#EXEMPLE: 
#parameters <- list(
#r_y = valeur1,
#u = valeur2,
#d0 = valeur3,
#q_c = valeur4,)
# Others

months_range <- 50
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
u_seed <- 4e-8 # Mutation rate per division WARNING
k_seed <- 1 # Kinetic coefficient WARNING
p0 <- 0.8 # Prob. T cell engages cancer cell
q_c <- 0.75 # Prob. cancer cell dies from encounter
q_t <- 0.5 # Prob. T cell survives encounter
tau_seed <- 1 # Duration of one T cell division

# Variables dependantes du temps
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# d_T Anti-leukemia T cell death rate see in patient Data
# s_T Anti-leukemia T cell supply rate patient Data
# c_n Decay in  rate of immmune responsivity patient Data

# Valeurs à t=0
#-------------------------------------------------------------------
#-------------------------------------------------------------------

get_y1_0 <- function (y0){
  return(a_y*y0/d1)
}

get_y2_0 <- function (y1){
  return (b_y*y1/d2)
}

get_y3_0 <- function (y2){
  return (c_y*y2/d3)
}

get_z1_0 <- function(z_0){
  return (a_z*z_0/d1)
}

get_z2_0 <- function(z1){
  return (b_z*z1/d2)
}

get_z3_0 <- function(z2){
  return (c_z*z2/d3)
}

get_Y0 <- function(s_T,d_T){
  return(s_T/d_T)
}

parameters <- list(
  lambda = lambda,
  d1 = d1,
  d2 = d2,
  d3 = d3,
  a_y = a_y,
  b_y = b_y,
  c_y = c_y,
  a_p_y = a_p_y,
  b_p_y = b_p_y,
  c_p_y = c_p_y,
  a_z = a_z,
  b_z = b_z,
  c_z = c_z,
  p0 = p0,
  q_c = q_c,
  q_t = q_t,
  # Added after getting the error
  d0 = d0_seed*lambda,
  r_z = r_z_seed,
  r_y = r_y_seed,
  k = k_seed ,
  tau = tau_seed,
  u = u_seed
)



