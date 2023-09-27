source("./utils.R")
source("./plot.R")
source("./params/params.R")

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


# Affichage des données
#----------------------------------------------------------------
#print("Displaying data")
#print(P4$time_month)
#print(P4$sfcs_well)
#plot_cy_cx(P1$time_month,P1$sfcs_well,"Leukemia SFCs - P1")
#plot_cy_cx(P4$time_month,P4$sfcs_well,"Leukemia SFCs - P4")
#plot_cy_cx(P12$time_month,P12$sfcs_well,"Leukemia SFCs - P12")
#plot_cy_cx_interpolated(P12$time_month,P12$sfcs_well,"Leukemia SFCs - P12 - interpolated")


# Interpolation
#----------------------------------------------------------------
#print("Interpolated")
#interpolated <- interpolate_data(P12$time_month,P12$sfcs_well)
#plot_cy_cx(interpolated$x,interpolated$y,"Leukemia SFCs - P12 - interpolated")
#plot_cy_cx(P12$time_month,P12$sfcs_well,"Leukemia SFCs - P12")