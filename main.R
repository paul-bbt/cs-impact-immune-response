source("./utils.R")
source("./plot.R")
source("./interpolate.R")
source("./params/params_1.R")
source("./params/patientsData.R")

# Affichage des données
#----------------------------------------------------------------
#print("Displaying data")
#print(P4$time_month)
#print(P4$sfcs_well)
#plot_cy_cx(P1$time_month,P1$sfcs_well,"Leukemia SFCs - P1")
#plot_cy_cx(P4$time_month,P4$sfcs_well,"Leukemia SFCs - P4")
#plot_cy_cx(P12$time_month,P12$sfcs_well,"Leukemia SFCs - P12")
#plot_cy_cx_interpolated(P12$time_month,P12$sfcs_well,"Leukemia SFCs - P12 - interpolated")

# Test class
P12 <- Plot$new(P12$time_month, P12$sfcs_well)
P12$plot_cx_cy("Graph P12")


# Interpolation
#----------------------------------------------------------------
#print("Interpolated")
#interpolated <- interpolate_data(P12$time_month,P12$sfcs_well)
#plot_cy_cx(interpolated$x,interpolated$y,"Leukemia SFCs - P12 - interpolated")
#plot_cy_cx(P12$time_month,P12$sfcs_well,"Leukemia SFCs - P12")