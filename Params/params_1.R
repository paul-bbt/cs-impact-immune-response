lambda <-0.75 # Fractional adjustment constant
d_0 <- 0 # SC Rate WARNING
d_1 <- 0.008*lambda # PC death Rate
d_2 <- 0.05*lambda # DC death Rate
d_3 <- lambda # TC death Rate
r_y <- 0 # Growth rate for nonresistant cells WARNING
a_y <- 1.6 # Rates without imatinib treatment
b_y <- 10
c_y <- 100 
a_p_y <- a_y/100 # Rates during imatinib treatment
b_p_y <- b_y/750 
c_p_y <- c_y 
r_z <- 0 # Growth rate for resistant cells WARNING
a_z <- a_y # Rates for resistant cells
b_z <- b_y 
c_z <- c_y
u <- 0 # Mutation rate per division WARNING
K <- 0 # Kinetic coefficient WARNING
p_0 <- 0.8 # Prob. T cell engages cancer cell
q_c <- 0.75 # Prob. cancer cell dies from encounter
q_t <- 0.5 # Prob. T cell survives encounter
tau <- 0 # Duration of one T cell division

# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

params_1 <- list(
  lambda = lambda,
  d_0 = d_0,
  d_1 = d_1,
  d_2 = d_2,
  d_3 = d_3,
  r_y = r_y,
  a_y = a_y,
  b_y = b_y,
  c_y = c_y,
  a_p_y = a_p_y,
  b_p_y = b_p_y,
  c_p_y = c_p_y,
  r_z = r_z,
  a_z = a_z,
  b_z = b_z,
  c_z = c_z,
  u = u,
  K = K,
  p_0 = p_0,
  q_c = q_c,
  q_t = q_t,
  tau = tau
)