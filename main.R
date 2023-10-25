source("./patientClass.R")
source("./data/patientsData.R")

# Test class
PClass <- PatientClass$new(P12)
PClass$model1()

#PClass$getModelScore()
#PClass$plotData("Plot data")
#PClass$plotModel()