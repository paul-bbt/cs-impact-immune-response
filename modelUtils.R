# Functions to be checked:

get_C <- function(day,y0,y1,y2,y3,z0,z1,z2,z3){
  return(sum(c(y0,y1,y2,y3,z0,z1,z2,z3))) 
}
get_C_n_tau <- function(day,y0,y1,y2,y3,z0,z1,z2,z3,n,tau){
  return(get_C(day-n*tau,y0,y1,y2,y3,z0,z1,z2,z3))
}
get_p_CT <- function (day,Patient,C,T,k){
  return(params$p_0*exp(-Patient$c_n*get_C())*k*T)
}
get_p_CT_n_tau <- function(day,Patient,C,T,k,n,tau){
  return(get_p_CT(day-n*tau,Patient,C,T,k))
}