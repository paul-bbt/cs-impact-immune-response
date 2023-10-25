# Functions to be checked:
get_C <- function(y0,y1,y2,y3,z0,z1,z2,z3){
  return(sum(c(y0,y1,y2,y3,z0,z1,z2,z3))) 
}
get_p_CT <- function (day,Patient,C,T,k){
  return(params$p0*exp(-Patient$c_n*C*k*T))
}