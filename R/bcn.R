
# 1 - BcnRegressor -------------------------------------------------------------------

BcnRegressor <- R6::R6Class(classname = "BcnRegressor",
                                inherit = learningmachine::BaseRegressor,
                                public = list(
                                  name = "RangerRegressor",
                                  type = "regression",
                                  model = NULL,
                                  X_train = NULL,
                                  y_train = NULL,
                                  engine = NULL, 
                                  params = NULL,
                                  initialize = function(name = "BcnRegressor",
                                                        type = "regression",
                                                        model = NULL,
                                                        X_train = NULL,
                                                        y_train = NULL,
                                                        engine = NULL, 
                                                        params = NULL){
                                    self$name <- name
                                    self$type <- type
                                    self$model <- model
                                    self$X_train <- X_train
                                    self$y_train <- y_train
                                    self$engine <- engine
                                    self$params <- params
                                  },
                                  fit = function(X, y, ...) {
                                    
                                    if(is_package_available("dfoptim") == FALSE)
                                      utils::install.packages("dfoptim",
                                                              repos = c(CRAN = "https://cloud.r-project.org"))
                                      
                                    if(is_package_available("bcn") == FALSE)
                                      utils::install.packages("bcn",
                                                              repos = c(techtonique = "https://techtonique.r-universe.dev"))
                                    
                                    stopifnot(is.numeric(y))
                                    
                                    self$X_train <- X
                                    self$y_train <- y
                                    self$params <- list(...)
                                    self$set_model(bcn::bcn(x = self$X_train, y = self$y_train, ...))
                                    self$set_engine(list(fit = bcn::bcn, 
                                                         predict = predict))
                                    return(base::invisible(self))
                                  },
                                  predict = function(X, level = NULL,
                                                     method = c("splitconformal",
                                                                "other"),
                                                     ...) {
                                    method <- match.arg(method)
                                    super$predict(X = X, level = level,
                                                  method = method)
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
                                     
                                     if(is_package_available("dfoptim") == FALSE)
                                       utils::install.packages("dfoptim",
                                                               repos = c(CRAN = "https://cloud.r-project.org"))
                                     
                                     if(is_package_available("bcn") == FALSE)
                                       utils::install.packages("bcn",
                                                               repos = c(techtonique = "https://techtonique.r-universe.dev"))
                                     
                                     stopifnot(is.factor(y) || is.integer(y))
                                     
                                     self$X_train <- X
                                     self$y_train <- as.factor(y)
                                     self$params <- list(...)
                                     self$set_model(bcn::bcn(x = self$X_train, y = self$y_train, ...))
                                     self$set_engine(list(fit = bcn::bcn, 
                                                          predict = predict))
                                     
                                     return(base::invisible(self))
                                   },
                                   predict = function(X, level = NULL,
                                                      method = c("splitconformal",
                                                                 "other"),
                                                      ...) {
                                     method <- match.arg(method)
                                     super$predict(X = X, level = level,
                                                   method = method)
                                   }
                                 ))

# predict_proba

# 3 - utils -------------------------------------------------------------------

