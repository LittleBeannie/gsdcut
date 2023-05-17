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
get_cut_date.ByEvent <- function(y, tte, ...) {
  n_analysis <- length(y$event)
  ans <- do.call(c, lapply(seq_along(y$event), function(i){
    cut_date <- simtrial::getCutDateForCount(x = tte, count = y$event[i])
  }))
  return(ans)
}

#' @export
get_cut_date.ByTime <- function(y, tte, ...) {
  return(y$time)
}

#' @export
get_cut_date.ByHybrid <- function(y, tte, ...) {
  
  flag_planned_total_duration <- !is.null(y$planned_total_duration)
  flag_time <- !is.null(y$time)
  flag_event <- !is.null(y$event)
  flag_min_followup <- !is.null(y$min_followup)
  
  # cut only by targeted events
  if(flag_event & flag_planned_total_duration + flag_time + flag_min_followup == 0){
    n_analysis <- length(y$event)
    ans <- do.call(c, lapply(seq_along(y$event), function(i){
      cut_date <- simtrial::getCutDateForCount(x = tte, count = y$event[i])
      return(cut_date)
    }))
    
  # cut only by calendar time
  }else if(flag_time & flag_planned_total_duration + flag_event + flag_min_followup == 0){
    ans <- y$time
    
  # cut by max(planned duration, targeted event)
  }else if(flag_planned_total_duration + flag_event == 2 & flag_time + flag_min_followup == 0){
    # get the cut date by targeted events
    n_analysis <- length(y$event)
    ans <- do.call(c, lapply(1:n_analysis, function(i){
      cut_date <- simtrial::getCutDateForCount(x = tte, count = y$event[i])
      return(cut_date)
    }))
    # if the event-based final analysis is longer than planned duration, take the former. 
    if(ans[n_analysis] < y$planned_total_duration){
      ans[n_analysis] <- planned_total_duration
    }
    
  # cut by max(minimum follow-up, targeted events)
  }else if(flag_min_followup + flag_event == 2 & flag_time + flag_planned_total_duration == 0){
    # get the cut date by targeted events
    n_analysis <- length(y$event)
    ans <- do.call(c, lapply(1:n_analysis, function(i){
      cut_date <- simtrial::getCutDateForCount(x = tte, count = y$event[i])
      return(cut_date)
    }))
    # if the event-based final analysis is longer than min follow up, take the former. 
    if(ans[n_analysis] < y$planned_total_duration){
      ans[n_analysis] <- enroll_time + min_followup
    }
  }
  
  return(ans)
}