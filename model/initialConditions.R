source("./params/params.R")
get_initial_conditions <- function (Patient){
  z0_0 <- 1e-10 # See page 2 on paper z0 = 0 or 1e-10
  y0_0 <- Patient$y0_0
  y1_0 <- get_y1_0(y0_0)
  y2_0 <- get_y2_0(y1_0)
  y3_0 <- get_y3_0(y2_0)
  z1_0 <- get_z1_0(z0_0)
  z2_0 <- get_z2_0(z1_0)
  z3_0 <- get_z3_0(z2_0)
  
  return(list(
    y0_0 = y0_0,
    y1_0 = y1_0,
    y2_0 = y2_0,
    y3_0 = y3_0,
    z0_0 = z0_0,
    z1_0 = z1_0,
    z2_0 = z2_0,
    z3_0 = z3_0,
    T_0 = Patient$s_t/Patient$d_t # See 5th page on the paper
  ))
}