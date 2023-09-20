
# 1 - GlmnetRegressor -------------------------------------------------------------------

GlmnetRegressor <- R6::R6Class(classname = "GlmnetRegressor",
                                inherit = learningmachine::BaseRegressor,
                                public = list(
                                  initialize = function(name = "GlmnetRegressor",
                                                        type = "regression"){
                                    self$name <- name
                                    self$type <- type
                                  },
                                  fit = function(X, y, ...) {
                                    if(is_package_available("glmnet") == FALSE)
                                      install.packages("glmnet")
                                    list_params <- list(...)
                                    stopifnot(!is.null(list_params$family))
                                    stopifnot(list_params$family %in% c("gaussian",
                                                                        "poisson",
                                                                        "cox",
                                                                        "mgaussian"))
                                    self$set_model(glmnet::glmnet(x = X, y = y,
                                                                    ...))
                                    return(base::invisible(self))
                                  },
                                  predict = function(X, ...) {
                                    predict(self$model, X, ...)
                                  }
                                ))

# 2 - GlmnetClassifier -------------------------------------------------------------------

GlmnetClassifier <- R6::R6Class(classname = "GlmnetClassifier",
                                 inherit = learningmachine::BaseClassifier,
                                 public = list(
                                   initialize = function(name = "GlmnetClassifier",
                                                         type = "classification"){
                                     self$name <- name
                                     self$type <- type
                                   },
                                   fit = function(X, y, ...) {
                                     if(is_package_available("glmnet") == FALSE)
                                       install.packages("glmnet")
                                     list_params <- list(...)
                                     stopifnot(!is.null(list_params$family))
                                     stopifnot(list_params$family %in% c("binomial",
                                                                         "multinomial"))
                                     self$set_model(glmnet::glmnet(x = X, y = y,
                                                                     ...))
                                     
                                     return(base::invisible(self))
                                   },
                                   predict = function(X, ...) {
                                     predict(self$model, X, ...)
                                   }
                                 ))

# predict_proba

# 3 - utils -------------------------------------------------------------------


