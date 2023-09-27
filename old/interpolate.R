interpolate_data <- function (cx,cy){
  new_time <- seq(0, length(cx), by = 1)
  interpolate <- approx(cx, cy, xout = new_time)
  return(interpolate)
}