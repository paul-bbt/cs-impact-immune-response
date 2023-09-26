source("./utils.R")
source("./plot.R")
source("./model.R")
source("./patient.R")
source("./interpolate.R")
source("./params/params_1.R")
source("./params/patientsData.R")

# Test class
P12 <- Patient$new(P12$time_month, P12$sfcs_well,P12$y_0_0, )
#P12$plot("Graph P12")