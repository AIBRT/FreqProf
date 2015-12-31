# Wrapper functions for applying linear interpolation, and correlation and 
# kolmogorov-smirnov tests to multiple variables in separate datasets.

approxm <- function(data, n, method = "linear") {
  # Linear interpolation for multiple variables in data
  # data = data.frame
  # n = desired interpolation length for each variable (column)
  # Returns data.frame with nrow = n
  newdata <- data.frame(matrix(nrow = n, ncol = ncol(data)))
  colnames(newdata) <- names(data)
  for(i in names(data)){
    newdata[, i] <- if(method == "linear") {
      approx(data[, i], n = n)$y
    } else if(method == "spline"){
      spline(data[, i], n = n)$y
    }
  }
  return(newdata)
}


ks.testm <- function(data1, data2, vars){
  # Compares multiple variables of the same name
  # in two data sets and returns in table format
  #
  a <- data.frame(matrix(nrow = length(vars), ncol = 2))
  for (i in 1:length(vars)){
    a[i, 1:2] <- unlist(ks.test(data1[, vars[i]], data2[, vars[i]]))[1:2]
  }
  a <- cbind(vars, a)
  names(a) <- c("vars", "D", "P")
  return(a)
}

#' Correlation test for multiple variables - of the same name - in separate
#' data.frames
#' 
#' @param data1 a data.frame with X variables
#' @param data2 a data.frame with the same X variables as data1
#' @param method a correlation method, either "pearson" or "spearman"
#'   
#' @return Returns a data.frame with correlation data
#' @export
#' 
#' @examples
cor.testm <- function(data1, data2, method) {
  # Correlation for multiple variables in data
  # data1 = data1
  # data2 = data2
  # method = correlation method
  # Returns data.frame NOTE: only works for Pearson. Need to adjust CI functions to include Spearman
  newdata <- list()
  x = 0
  for(i in names(data1)){
    x = x + 1
    newdata[[x]] <- cor.test(data1[, i], data2[, i], method = method)
  }
  
  # Reshape data
  newdata <- data.frame(t(sapply(newdata, c)))
  newdata <- apply(newdata, 2, unlist)
  
  # Split conf.int (double length vector) and remove original
  if(method == "pearson") {
    ci <- split(newdata$conf.int, 1:2)
    ci.min <- ci[1]
    ci.max <- ci[2]
    newdata$conf.int <- NULL
  }
  
  # Add and rearrange variables for readability
  newdata <- data.frame(var    = names(data1),
                        cor    = newdata[["estimate"]],
                        ci.min = ifelse(method == "pearson", ci.min, NA),
                        ci.max = ifelse(method == "pearson", ci.max, NA),
                        p      = newdata[["p.value"]],
                        stat   = newdata[["statistic"]],
                        df     = ifelse(method == "pearson", newdata[["parameter"]], NA),
                        alt    = newdata[["alternative"]],
                        method = method)
  names(newdata)[3:4] <- c("ci.min", "ci.max")
  return(newdata)
}
