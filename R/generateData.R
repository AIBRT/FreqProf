#' Generate a sequence of dates
#' 
#' @param startDate a character string in the format "YYYY-MM-DD"
#' @param nDays length of output vector
#' @param by days or weeks
#'   
#' @return Sequence of dates starting at startDate with length nDays by daily or
#'   weekly increments
#' @export
#' 
#' @examples 
#' generate_dates("2016-06-05", 31, by = "day")
#'
generate_dates = function(startDate, nDays, by = "day") {
  
  startDate = as.Date(startDate)
  nDays = nDays
  data1 = seq(startDate, by = by, length.out = nDays)
  return(data1)
}


#' Title
#'
#' @param nBehaviors 
#' @param nrow 
#' @param prStart 
#' @param prEnd 
#' @param prBy 
#'
#' @return
#' @export
#'
#' @examples
generate_behaviors = function(nrow, nBehaviors,
                              prStart = 0.1, prEnd = 0.3, prBy = 0.001){
  
  pr = sample(seq(prStart, prEnd, prBy), 
              nBehaviors, 
              replace = T)
  data1 = sapply(pr, function(x) rbinom(nrow, 1, x))
  return(data1)
}
