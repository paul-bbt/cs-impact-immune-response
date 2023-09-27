check <- function(c1,c2){
  return(length(c1) == length(c2))
}

# --------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------

rmse <- function(c1,c2){
  if(!check(c1,c2)){
    stop("Error, check length")
  }
  mean_squared_differences <- mean((c1 - c2)^2)
  return (sqrt(mean_squared_differences))
}


mae <- function(c1,c2){
  if(!check(c1,c2)){
    stop("Error")
  }
  return(mean(abs(c1 - c2)))
}


# Example
#print(mae(c(1, 2, 3),c(1.1, 1.9, 3.2)))