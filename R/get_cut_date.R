#' Simulate stratified time-to-event outcome randomized trial
#'
#' @param x the object
#' @param ... Additional arguments (not used).
#'
#' @return TBA
#'
#' @export
get_cut_date <- function(y, ...) {
  UseMethod("get_cut_date", y)
}

#' @export
get_cut_date <- function(y, cut_class, ...) {
  
  input_time <- !is.null(cut_class$time)
  input_event <- !is.null(cut_class$event)
  input_min_followup <- !is.null(cut_class$min_followup)
  
  ans <- NULL
  # cut by planned duration
  if(input_time == 1 & input_event + input_min_followup == 0){
    ans <- cut_class$time
    
  # cut by targeted events
  } else if (input_event == 1 & input_time + input_min_followup == 0){
    ans <- simtrial::getCutDateForCount(x = y, count = cut_class$event)
    
  # cut by minimal follow-up
  } else if(input_min_followup == 1 & input_time + input_event == 0){
    ans <- enroll_time + cut_class$min_followup
    
  # cut by max of planned duration, targeted events
  } else if(input_event + input_time == 2 & input_min_followup == 0){
    date_by_event <- simtrial::getCutDateForCount(x = y, count = cut_class$event)
    ans <- max(date_by_event, cut_class$time)
    
  # cut by max of minimum follow-up, targeted events
  } else if(input_min_followup + input_event == 2 & input_time == 0){
    min_followup_enroll <- enroll_time + cut_class$min_followup
    date_by_event <- simtrial::getCutDateForCount(x = y, count = cut_class$event)
    ans <- max(min_followup_enroll, date_by_event)
  }
  return(ans)
}