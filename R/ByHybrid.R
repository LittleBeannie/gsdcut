#' Cut by time
#'
#' @description A class for event-based cut
#'
#' @export
ByHybrid <- R6::R6Class(
  "ByHybrid",
  inherit = Cut,
  public = list(
    #' @field time calendar time to cut the interim/final analysis.
    time = NULL,
    #' @field event target events to cut the interim/final analysis.
    event = NULL,
    #' @field planned_total_duration planned duration cutoff
    planned_total_duration = NULL,
    #' @field min_followup minimum follow-up duration
    min_followup = NULL
  )
)