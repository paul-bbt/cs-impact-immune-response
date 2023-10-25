source("./patientClass.R")
source("./data/patientsData.R")

# For plotting
dev.off() # Closing all graphics
par(mfrow=c(1,2))

# Test class
PClass <- PatientClass$new(P1)
PClass$plotData()

#PClass$modelNonLinear()
#PClass$plotModelNonLinear()

PClass$modelOne()
PClass$plotModelOne()
#PClass$getModelScore()