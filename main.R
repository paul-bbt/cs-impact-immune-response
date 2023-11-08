# rm(list=ls())
source("./patientClass.R")
source("./data/patientsData.R")

# Recherche d'un papier similaire
# https://scholar.google.com/scholar?hl=fr&as_sdt=0%2C5&q=chronic+myelogenous+leukemia+immune+response+modeling&btnG=

# For plotting
# For plotting
dev.off()
par(mar = c(1, 1, 1, 1))

# Test class
PClass <- PatientClass$new(P4)
PClass$modelOne()
PClass$plotModelOne()
PClass$plotData()
print("Done")
# PClass$getModelScore() not working yet