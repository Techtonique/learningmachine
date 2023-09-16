
# 1 - BcnRegressor -------------------------------------------------------------------

BcnRegressor <- R6::R6Class(classname = "BcnRegressor",
                                inherit = learningmachine::BaseRegressor,
                                public = list(
                                  initialize = function(name = "BcnRegressor",
                                                        type = "regression"){
                                    self$name <- name
                                    self$type <- type
                                  },
                                  fit = function(X, y, ...) {
                                    if(is_package_available("bcn") == FALSE)
                                      try(install.packages("bcn",
                                                           repos = c(techtonique = "https://techtonique.r-universe.dev")),
                                          silent = TRUE
                                      )
                                    
                                    stopifnot(is.numeric(y))
                                    
                                    self$set_model(bcn::bcn(x = X, y = y, ...))
                                  },
                                  predict = function(X, ...) {
                                    predict(self$model, X, ...)
                                  }
                                ))

# 2 - BcnClassifier -------------------------------------------------------------------

BcnClassifier <- R6::R6Class(classname = "BcnClassifier",
                                 inherit = learningmachine::BaseClassifier,
                                 public = list(
                                   initialize = function(name = "BcnClassifier",
                                                         type = "classification"){
                                     self$name <- name
                                     self$type <- type
                                   },
                                   fit = function(X, y, ...) {
                                     if(is_package_available("bcn") == FALSE)
                                       try(install.packages("bcn",
                                                            repos = c(techtonique = "https://techtonique.r-universe.dev")),
                                           silent = TRUE
                                       )
                                     
                                     stopifnot(is.factor(y) || is.integer(y))
                                     
                                     self$set_model(bcn::bcn(x = X, y = y, ...))
                                   },
                                   predict = function(X, ...) {
                                     predict(self$model, X, ...)
                                   }
                                 ))

# 3 - utils -------------------------------------------------------------------

