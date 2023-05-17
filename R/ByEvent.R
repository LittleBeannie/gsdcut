#' Cut by event
#'
#' @description A class for event-based cut
#'
#' @export
ByEvent <- R6::R6Class(
  "ByEvent",
  inherit = Cut,
  public = list(
    #' @field event number of event to cut the interim/final analysis.
    event = NULL,
    
    #' @description
    #' Validate input parameters.
    #'
    #' @param event number of event to cut the interim/final analysis.
    validate = function(event) {
      if (!is.numeric(event)) {
        stop("`event` must be numeric.", call. = FALSE)
      }
      if (any(event <= 0)) {
        stop("`event` must be a positive number.", call. = FALSE)
      }
      if (any(diff(event)<= 0)) {
        stop("`event` must be a vector of increasing positive numbers.", call. = FALSE)
      }
    },
    
    #' @description TBA
    #'
    #' @param event number of event to cut the interim/final analysis.
    #'
    #' @return A new `Exponential` object.
    initialize = function(event) {
      self$validate(event)
      self$event <- event
    },
    
    
    #' @description TBA
    summary = function() {
      paste0(
        "There are ", length(self$event), 
        " analysis (including final analysis) occuring when there are ",
        paste0(self$event, collapse = ", "), " events."
      )
    }
  )
)