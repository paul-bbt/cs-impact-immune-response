library(R6)

plot_cx_cy <- function(cx, cy, title = "Graph", x_axis_title = "x_title", y_axis_title = "y_title") {
  if (!is.numeric(cy) || length(cy) < 2) {
    stop("Error: cy must be a numeric vector and length >= 2.")
  }
  a <- c(0, max(cx))
  plot(cx, cy, 
       main = title,
       xlab = x_axis_title,
       ylab = y_axis_title,
       col = "blue",
       pch = 19,
       xlim = c(0, max(cx)),
       ylim = c(0, max(cy))
  )
  lines(cx, cy, col = "blue", type = "l")
}

Plot <- R6Class(
  "Plot",
  public = list(
    cx = NULL,
    cy = NULL,
    
    initialize = function(cx, cy) {
      self$cx <- cx
      self$cy <- cy
    },
    
    plot_cx_cy = function(title = "Graph", x_axis_title = "x_title", y_axis_title = "y_title") {
      plot_cx_cy(self$cx, self$cy, title, x_axis_title, y_axis_title)
    }
  )
)
