library(R6)
source("./utils/scores.R")
source("./model_1/model_v2.R")
source("./model_2/program_v2.R")
source("./model_non_linear/nonlinear_v1.R")
source("./utils/plot.R")

plot_cx_cy(c(0, 1), c(0, 1), "title", "x_axis_title", "y_axis_title")

PatientClass <- R6Class(
  "Patient",
  public = list(
    Patient = NULL,
    modelOneOut = NULL,
    modelScore = NULL,
    initialize = function(Patient) {
      self$Patient <- Patient
    },
    modelOne = function() {
      self$modelOneOut <- modelPierre(self$Patient)
    },
    getModelScore = function() {
      if (is.null(self$modelOneOut)) {
        stop("No model loaded")
      }
      
      valid_indices <- self$Patient$time_days != 0
      
      y <- self$modelOneOut[, "T"]
      modelData <- y[self$Patient$time_days[valid_indices]]
      patientData <- self$Patient$sfcs_well[valid_indices]
      
      rmse <- rmse(modelData, patientData)
      mae <- mae(modelData, patientData)
      
      print("Study of the model:")
      print(paste("Rmse:", rmse))
      print(paste("Mae:", mae))
      
      self$modelScore <- rmse
    },
    plotData = function(title = "Patient", x_axis_title = "Temps [m]", y_axis_title = "T cells [mol.L-1]") {
      plot_cx_cy(self$Patient$time_days, self$Patient$sfcs_well, title, x_axis_title, y_axis_title)
    },
    plotModelOne = function(title = "Model One", x_axis_title = "Temps [m]", y_axis_title = "T cells [mol.L-1]") {
      if (is.null(self$modelOneOut)) {
        stop("Model one not loaded")
      }
      plot_cx_cy(self$modelOneOut[, "time"], self$modelOneOut[, "T"], title, x_axis_title, y_axis_title, 2)
    }
  )
)