library(R6)
source("./utils/scores.R")
source("./model_1/model_v2.R")
source("./model_2/program_v2.R")
source("./model_non_linear/nonlinear_v1.R")
source("./utils/plot.R")

plot_cx_cy(c(0,1), c(0,1), "title", "x_axis_title", "y_axis_title")
PatientClass <- R6Class(
  "Patient",
  
  public = list(
    Patient = NULL,
    modelOneOut = NULL,
    modelNonLinearOut = NULL,
    modelScore = NULL,
    initialize = function(Patient) {
      self$Patient <- Patient
  },
  
  # Model by Pierre
  # -------------------------------------------------------------------------------------------------
  modelOne = function (){
    # self$modelOut <- model1(self$Patient)
    self$modelOneOut <- modelPierre(self$Patient)
  },
  
  # Non linear model for I.3)
  modelNonLinear = function (){
    self$modelNonLinearOut <-nonLinearModel(self$Patient$time_days)
  },
  
  # Model score between non-linear model and data
  # À sortir dans une fonction indépendante
  # -------------------------------------------------------------------------------------------------
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
  # -------------------------------------------------------------------------------------------------
  plotData = function(title = "Patient", x_axis_title = "Temps [m]", y_axis_title = "T cells [mol.L-1]") {
    plot_cx_cy(self$Patient$time_days, self$Patient$sfcs_well, title, x_axis_title, y_axis_title)
  },
  
  # T modelOne displayed with a line
  # -------------------------------------------------------------------------------------------------
  plotModelOne = function(title = "Model One", x_axis_title = "Temps [m]", y_axis_title = "T cells [mol.L-1]") {
    if(is.null(self$modelOneOut)){
      stop("Model one not loaded")
    }
    plot_cx_cy(self$modelOut[, "time"], self$modelOut[, "T"], title, x_axis_title, y_axis_title, 2)
  },
  
  # T modelNonLinear displayed with a line
  # -------------------------------------------------------------------------------------------------
  plotModelNonLinear = function(title = "Non Linear Model", x_axis_title = "Temps [m]", y_axis_title = "T cells [mol.L-1]") {
    if(is.null(self$modelNonLinearOut)){
      stop("Non linear model not loaded")
    }
    plot_cx_cy(self$modelNonLinearOut[, "time"], self$modelNonLinearOut[, "T"], title, x_axis_title, y_axis_title, 2)
  }
  )
)
