
# 1 - RangerRegressor -------------------------------------------------------------------

RangerRegressor <- R6::R6Class(classname = "RangerRegressor",
                               inherit = learningmachine::BaseRegressor,
                               public = list(
                                 name = "RangerRegressor",
                                 type = "regression",
                                 model = NULL,
                                 X_train = NULL,
                                 y_train = NULL,
                                 engine = NULL, 
                                 params = NULL,
                                 initialize = function(name = "RangerRegressor",
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
                                   if(is_package_available("ranger") == FALSE)
                                     install.packages("ranger",
                                                      repos = c(CRAN = "https://cloud.r-project.org"))
                                   self$X_train <- X
                                   self$y_train <- y
                                   self$params <- list(...)
                                   self$set_model(fit_func_ranger(x = self$X_train, y = self$y_train, ...))
                                   self$set_engine(list(fit = fit_func_ranger, 
                                                        predict = predict_func_ranger))
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


# 2 - RangerClassifier -------------------------------------------------------------------

RangerClassifier <- R6::R6Class(classname = "RangerClassifier",
                                inherit = learningmachine::BaseClassifier,
                                public = list(
                                  initialize = function(name = "RangerClassifier",
                                                        type = "classification",
                                                        engine = NULL){
                                    self$name <- name
                                    self$type <- type
                                    self$engine <- engine
                                  },
                                  fit = function(X, y, ...) {
                                    if(is_package_available("ranger") == FALSE)
                                      install.packages("ranger",
                                                       repos = c(CRAN = "https://cloud.r-project.org"))
                                    self$set_engine(fit_func_ranger)
                                    self$set_model(fit_func_ranger(x = X, y = y, ...))
                                    return(base::invisible(self))
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

