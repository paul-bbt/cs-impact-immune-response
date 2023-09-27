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
}
