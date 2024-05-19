
fit_func_extratrees_regression <- function(x, y, ...) {
  df <-
    data.frame(y = y, x)  # naming of columns is mandatory for `predict`
  ranger::ranger(
    y ~ .,
    data = df,
    splitrule = "extratrees",
    replace = FALSE,
    sample.fraction = 1,
    ...
  )
}

# fit_func_extratrees_classification <- function(x, y, ...) {
#   df <-
#     data.frame(y = y, x)  # naming of columns is mandatory for `predict`
#   ranger::ranger(
#     y ~ .,
#     data = df,
#     splitrule = "extratrees",
#     replace = FALSE,
#     sample.fraction = 1,
#     probability = TRUE,
#     ...
#   )
# }

predict_func_extratrees <- function(obj, newx, ...) {
  if (is.null(colnames(newx)))
    colnames(newx) <-
      paste0("X", 1:ncol(newx))  # mandatory, linked to df in fit_func
  
  res <-
    try(predict(object = obj, data = newx, ...)$predictions, silent = TRUE)
  # only accepts a named newx
  if (inherits(res, "try-error")) {
    res <-
      try(predict(object = obj, data = matrix(newx, nrow = 1), ...)$predictions,
          silent = TRUE)
  }
  return(res)
}
