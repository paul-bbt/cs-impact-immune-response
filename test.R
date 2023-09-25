source("./utils.R")
source("./plot.R")
source("./params/params_1.R")

# EX1
current_directory <- getwd()
print(paste("Le répertoire de travail actuel est :", current_directory))
# To get the utils functions
source("./utils.R")

# EX2
cy <- c(1, 2, 3, 4, 5)
#plot_cy(cy, "Title")

# EX3
print(params_1$b)