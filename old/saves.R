# Changed
get_d0 <- function (day){
  if (day == 0 ){return(1)}
  return(d0_seed*lambda/day)
}
get_r_z <- function (day){
  if (day == 0 ){return(1)}
  return(r_z_seed/day)
}
get_r_y <- function(day){
  if (day == 0 ){return(1)}
  return(r_y_seed/day)
}
get_K <- function (day){
  if (day == 0 ){return(1)}
  return(K_seed/day)
  #return((K_seed/KuL)/day)
  # Check here !
}
get_tau <- function (day_unit){
  return(tau_seed*day_unit)
}

# Others
get_u <- function (division){
  if (division == 0 ){return(1)}
  return(u_seed/division)
}
