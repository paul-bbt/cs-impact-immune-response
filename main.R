source("./patientClass.R")
source("./data/patientsData.R")

# For plotting
par(mfrow=c(1,2))

# Test class
PClass <- PatientClass$new(P1)
PClass$plotData("Plot data")
PClass$model1()
PClass$plotModel()
#PClass$getModelScore()