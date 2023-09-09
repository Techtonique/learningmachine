
# 1 - XgboostRegressor -------------------------------------------------------------------

XgboostRegressor <- R6::R6Class(classname = "XgboostRegressor",
                               inherit = learningmachine::BaseRegressor,
                               public = list(
                                 initialize = function(name = "XgboostRegressor",
                                                       type = "regression"){
                                   self$name <- name
                                   self$type <- type
                                 },
                                 fit = function(X, y, ...) {
                                   self$set_model(fit_func_xgboost(x = X, y = y,
                                                                   ...))
                                 },
                                 predict = function(X, ...) {
                                   predict(self$model, X, ...)
                                 }
                               ))

# 2 - XgboostClassifier -------------------------------------------------------------------

XgboostClassifier <- R6::R6Class(classname = "XgboostClassifier",
                                inherit = learningmachine::BaseClassifier,
                                public = list(
                                  initialize = function(name = "XgboostClassifier",
                                                        type = "classification"){
                                    self$name <- name
                                    self$type <- type
                                  },
                                  fit = function(X, y, ...) {
                                    self$set_model(fit_func_xgboost(x = X, y = y,
                                                                    ...))
                                  },
                                  predict = function(X, ...) {
                                    predict(self$model, X, ...)
                                  }
                                ))

# 3 - utils -------------------------------------------------------------------

fit_func_xgboost <- function(x, y, ...)
{
  xgboost::xgboost(data = x, label = y, ...)
}

