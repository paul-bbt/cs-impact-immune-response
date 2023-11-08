source("./patientClass.R")
source("./data/patientsData.R")

# Recherche d'un papier similaire
# https://scholar.google.com/scholar?hl=fr&as_sdt=0%2C5&q=chronic+myelogenous+leukemia+immune+response+modeling&btnG=

# For plotting
dev.off() # Closing all graphics
par(mfrow=c(1,2))

# Test class
PClass <- PatientClass$new(P1)
PClass$plotData()
PClass$modelOne()
PClass$plotModelOne()
# PClass$getModelScore() not working yet