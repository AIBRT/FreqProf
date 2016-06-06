generate_dates = function(startDate, nDays, by = "day") {
  
  startDate = as.Date(startDate)
  nDays = nDays
  data1 = seq(startDate, by = by, length.out = nDays)
  return(data1)
}


generate_behaviors = function(nBehaviors, nrow,
                              prStart = 0.1, prEnd = 0.3, prBy = 0.001){
  
  pr = sample(seq(prStart, prEnd, prBy), 
              nBehaviors, 
              replace = T)
  data1 = sapply(prob, function(x) rbinom(nrow, 1, x))
  return(data1)
}
