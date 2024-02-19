fit_xgboost_regression <- function(x, y, ...) {
  f_xgboost <- function(x, y, ...)
  {
    xgboost::xgboost(data = x,
                     label = y,
                     ...)
  }
  do.call(what = f_xgboost,
          args = c(list(x = x,
                        y = y),
                   ...))
}
