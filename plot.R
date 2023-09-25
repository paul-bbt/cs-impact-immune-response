plot_cy_cx <- function(cy, cx =1:length(cy),  title = "Graph", x_axis_title, y_axis_title) {
  if (!is.numeric(cy) || length(cy) < 2) {
    stop("Error : cy must be a numeric vector and length >= 2.")
  }
  plot(cx, cy, 
       main = title,
       xlab = x_axis_title,
       ylab = y_axis_title,
       col = "blue",
       pch = 19,
       xlim = c(0, length(cy) + 1),
       ylim = range(0, cy)
  )
  lines(cx, cy, col = "blue", type = "l")
}

# -------------------------------------------------------------
# -------------------------------------------------------------

# Don't use
plot_matrix_heat_map <- function(matrix, title = "Matrix heat_map") {
  image(matrix, col = rev(colorRampPalette(c("lightblue", "darkblue"))(12)),xaxt = "n", yaxt = "n", main = title)
  #legend("topright", legend = unique(matrix), fill = colorRampPalette(c("lightblue", "darkblue"))(12), title = "Values")
}

# -------------------------------------------------------------
# -------------------------------------------------------------

print_matrix <- function(matrix) {
  print(matrix)
}

# -------------------------------------------------------------
# -------------------------------------------------------------

# Example 1
# cy <- c(1, 2, 3, 4, 5)
# plot_cy(cy)
# Example 2 & 3
#matrix_1 <- matrix(1:200, nrow = 10)
#print_matrix(matrix_1)
#plot_matrix_heat_map(matrix_1)
