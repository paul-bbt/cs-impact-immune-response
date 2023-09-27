source("./patientClass.R")
source("./params/patientsData.R")

# Test class
P12Class <- PatientClass$new(P12)
P12Class$plotData("Plot data")
P12Class$model1()
P12Class$plotModel()
P12Class$getModelScore()

# Ajouter une méthode pour appliquer la méthode des moindres carrés afin de valider la modélisation
# À propos de l'analyse de sensibilité: 
# - Modéliser l'incertitutde des facteurs (Page 67 du cours) et faire un camenbert pour comprendre 
#       l'ordre d'importances des facteurs.