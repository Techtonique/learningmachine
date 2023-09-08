
# 1 - RangerRegressor -------------------------------------------------------------------

RangerRegressor <- R6::R6Class(classname = "RangerRegressor",
                               inherit = learningmachine::BaseRegressor,
                               public = list(
                                 initialize = function(name = "RangerRegressor",
                                                       type = "regression"){
                                   self$name <- name
                                   self$type <- type
                                 },
                                 fit = function(X, y, ...) {
                                   self$set_model(fit_func_ranger(x = X, y = y, ...))
                                 },
                                 predict = function(X) {
                                   predict_func_ranger(obj = self$model,
                                                       newx = X)
                                 }
                               ))

# 2 - RangerClassifier -------------------------------------------------------------------

RangerClassifier <- R6::R6Class(classname = "RangerClassifier",
                               inherit = learningmachine::BaseClassifier,
                               public = list(
                                 initialize = function(name = "RangerClassifier",
                                                       type = "classification"){
                                   self$name <- name
                                   self$type <- type
                                 },
                                 fit = function(X, y, ...) {
                                   self$set_model(fit_func_ranger(x = X, y = y, ...))
                                 },
                                 predict = function(X, type = "response") {
                                   predict_func_ranger(self$model, newx = X,
                                                       type = type)
                                 }
                               ))

# 3 - utils -------------------------------------------------------------------

fit_func_ranger <- function(x, y, ...)
{
  df <- data.frame(y=y, x) # naming of columns is mandatory for `predict`
  ranger::ranger(y ~ ., data=df, ...)
}

predict_func_ranger <- function(obj, newx, ...)
{
  if(is.null(colnames(newx)))
    colnames(newx) <- paste0("X", 1:ncol(newx)) # mandatory, linked to df in fit_func
  predict(object=obj, data=newx, ...)$predictions # only accepts a named newx
}

