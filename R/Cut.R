#' Cut class
#'
#' A class for cuttings in group sequantial designs.
#'
#' @export
Cut <- R6::R6Class(
  "Cut",
  public = list(
    #' @field time planned duration cutoff
    time = NULL,
    #' @field event target events to cut the interim/final analysis.
    event = NULL,
    #' @field min_followup minimum follow-up duration
    min_followup = NULL,
    
    #' @description
    #' Validate input parameters.
    validate = function() {
      stop("Not implemented", call. = FALSE)
    },
    
    #' @description
    #' Create a new exponential distribution object.
    #'
    #' @param time calendar time to cut the interim/final analysis.
    #' @param event target events to cut the interim/final analysis.
    #' @param min_followup minimum follow-up duration
    #'
    #' @return A new `Cut` object.
    initialize = function(time, event, min_followup) {
      #self$validate(rate)
      self$time <- if(missing(time)){NULL}else{time}
      self$event <- if(missing(event)){NULL}else{event}
      self$min_followup <- if(missing(min_followup)){NULL}else{min_followup}
    },
    
    #' @description
    #' Summarize the distribution.
    summary = function() {
      input_time <- !is.null(self$time)
      input_event <- !is.null(self$event)
      input_min_followup <- !is.null(self$min_followup)
      
      ans <- NULL
      # cut by planned duration
      if(input_time == 1 & input_event + input_min_followup == 0){
        ans <- paste0("The analysis is cut at the ", self$time, "-th month.")
      # cut by targeted events
      } else if (input_event == 1 & input_time + input_min_followup == 0){
        ans <- paste0("The analysis is cut when there are ", self$event, " events.")
      # cut by minimal follow-up
      } else if(input_min_followup == 1 & input_time + input_event == 0){
        ans <- paste0("The analysis is cut when the minimal follow-up time arrives(", self$min_followup, " months after enrollment.")
      # cut by max of planned duration, targeted events
      } else if(input_event + input_time == 2 & input_min_followup == 0){
        ans <- paste0("The analysis is cut by the maximal of planned duration(", self$time, " months) and targeted events (", self$event, " events).")
      # cut by max of minimum follow-up, targeted events
      } else if(input_min_followup + input_event == 2 & input_time == 0){
        ans <- paste0("The analysis is cut by maximal of targeted events(", self$event, " events) and minimal follow-up(", self$min_followup, " months after enrollment).")
      }
      return(ans)
    }
    
  )
)


#' @export
summary.Cut <- function(x, ...) {
  x$summary(...)
}
