interpolate_data <- function (cx,cy){
  new_time <- seq(0, length(cx), by = 1)
  interpolate_y <- approx(cx, cy, xout = new_time)
  return(interpolate_y)
}

