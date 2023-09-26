library(R6)
source("./plot.R")
source("./utils.R")
source("./model.R")
source("./interpolate.R")

Patient <- R6Class(
  "Patient",
  public = list(
    cx = NULL,
    cy = NULL,
    y_0 = NULL,

    initialize = function(cx, cy, y_0) {
      self$cx <- cx
      self$cy <- cy
      self&y_0 <- y_0
    },
    
    plot = function(title = "Plot", x_axis_title = "x_title", y_axis_title = "y_title") {
      plot_cx_cy(self$cx, self$cy, title, x_axis_title, y_axis_title)
    },
    
    interpolate = function (){
      interpolate_data(self$cx, self$cy)
    },
    
    plot_interpolate = function(title = "Plot interpolate"){
      data <- interpolate_data(self$cx, self$cy)
      plot_cx_cy(data$x,data$y, title)
    }
    
    # Il manque une méthode pour afficher l'interpolation sur la courbe des points
    
  )
)
