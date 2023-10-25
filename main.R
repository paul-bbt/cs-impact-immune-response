source("./patientClass.R")
source("./params/patientsData.R")

# Test class
PClass <- PatientClass$new(P12)
PClass$model1() # Works with P1, P4 & P12
#PClass$getModelScore()
#PClass$plotData("Plot data")
#PClass$plotModel()

# Modéliser l'incertitutde des facteurs (Page 67 du cours) et faire un camenbert pour comprendre 
# l'ordre d'importances des facteurs.