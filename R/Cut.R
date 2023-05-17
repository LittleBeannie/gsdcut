#' Cut class
#'
#' A class for cuttings in group sequantial designs.
#'
#' @export
Cut <- R6::R6Class(
  "Cut",
  public = list(
    #' @description
    #' Validate input parameters.
    validate = function() {
      stop("Not implemented", call. = FALSE)
    },
    
    #' @description
    #' Summarize the distribution.
    summary = function() {
      stop("Not implemented", call. = FALSE)
    }
  )
)


#' @export
summary.Cut <- function(x, ...) {
  x$summary(...)
}
