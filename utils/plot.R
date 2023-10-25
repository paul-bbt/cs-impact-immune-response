plot_cx_cy <- function(cx, cy, title = "Graph", x_axis_title = "x_title", y_axis_title = "y_title", add = FALSE) {
#  if (add != 0) {
#    par(mfrow=c(floor(add / 2),add))
#  }
  
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
}

pieChart <- function(cValues, cNames, title = "Pie Chart"){
  graphics::pie(cValues, labels = cNames, main = title, radius = 1)
}

# Example with pie function
#values <- c(10, 60, 30,23)
#names <- c("A","B","C","D")
#pieChart(values,names)