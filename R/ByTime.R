#' Cut by time
#'
#' @description A class for event-based cut
#'
#' @export
ByTime <- R6::R6Class(
  "ByTime",
  inherit = Cut,
  public = list(
    #' @field time calendar time to cut the interim/final analysis.
    time = NULL,
    
    #' @description
    #' Validate input parameters.
    #'
    #' @param time calendar time to cut the interim/final analysis.
    validate = function(time) {
      if (!is.numeric(time)) {
        stop("`time` must be numeric.", call. = FALSE)
      }
      if (any(time <= 0)) {
        stop("`time` must be a positive number.", call. = FALSE)
      }
      if (any(diff(time)<= 0)) {
        stop("`time` must be a vector of increasing positive numbers.", call. = FALSE)
      }
    },
    
    #' @description TBA
    #'
    #' @param time calendar time to cut the interim/final analysis.
    #'
    #' @return TBA
    initialize = function(time) {
      self$validate(time)
      self$time <- time
    },
    
    
    #' @description TBA
    summary = function() {
      paste0(
        "There are ", length(self$time), 
        " analysis (including final analysis) occuring when there are ",
        paste0(self$time, collapse = "-th, "), "-th month."
      )
    }
  )
)