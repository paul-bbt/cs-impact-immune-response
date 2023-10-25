func <- function (x){
  a <- 1
  b <- 1
  return(a*x+b) 
}

nonLinearModel <- function(time_days){
  fx <-func(time_days)
  data <- data.frame(time = time_days, T = fx)
  return(data)
}