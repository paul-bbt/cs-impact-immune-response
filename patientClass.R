library(R6)
source("./utils/plot.R")
source("./model/model_2.R")
source("./utils/scores.R")

PatientClass <- R6Class(
  "Patient",
  public = list(
    Patient = NULL,
    modelOut = NULL,
    modelScore = NULL,
    initialize = function(Patient) {
      self$Patient <- Patient
    },
    
    # The paper' model 
    model1 = function() {
      self$modelOut <- model1(self$Patient)
    },
    
    # Model score
    getModelScore = function (){
      
      if(is.null(self$modelOut)){
        stop("No model loaded")
      }
      
      # Filter to delete the 0
      valid_indices <- self$Patient$time_days != 0
      
      # On prend les ordonnées du modèles au niveau des abcisses des données patients
      y <- self$modelOut[, "T"]
      modelData <- y[self$Patient$time_days[valid_indices]]
      patientData <- self$Patient$sfcs_well[valid_indices]
      
      # We get the scores here
      rmse <- rmse(modelData,patientData)
      mae <- mae(modelData,patientData)
      
      print("Study of the model:")
      print(paste("Rmse:",rmse))
      print(paste("Mae:",mae))
      
      self$modelScore=rmse
      
    },
    
    # Data displayed with dots
    plotData = function(title = "Plot", x_axis_title = "x_title", y_axis_title = "y_title") {
      plot_cx_cy(self$Patient$time_days, self$Patient$sfcs_well, title, x_axis_title, y_axis_title)
    },
    
    # T displayed with a line
    plotModel = function(){
      if(is.null(self$modelOut)){
        stop("No model loaded")
      }
      lines(self$modelOut[, "time"], self$modelOut[, "T"], col="violet")
    }
    
  )
)
