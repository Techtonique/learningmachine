
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
                                   if(is_package_available("xgboost") == FALSE)
                                     install.packages("xgboost",
                                                      repos = c(CRAN = "https://cloud.r-project.org"))
                                   list_params <- list(...)
                                   stopifnot(!is.null(list_params$params))
                                   stopifnot(!is.null(list_params$params$objective))
                                   stopifnot(sum(grepl(list_params$params$objective,
                                                       c("reg:squarederror",
                                                         "reg:squaredlogerror",
                                                         "reg:pseudohubererror",
                                                         "count:poisson",
                                                         "survival:cox",
                                                         "survival:aft",
                                                         "rank:pairwise",
                                                         "rank:ndcg",
                                                         "rank:map",
                                                         "reg:gamma",
                                                         "reg:tweedie"))) >= 1)
                                   self$X_train <- X
                                   self$y_train <- y
                                   self$params <- list(...)
                                   self$set_model(fit_func_xgboost(x = self$X_train, 
                                                                   y = self$y_train,
                                                                   ...))
                                   self$set_engine(list(
                                     fit = function(x, y) fit_func_xgboost(x, y, ...),
                                     predict = predict
                                   ))
                                   return(base::invisible(self))
                                 },
                                 predict = function(X, level = NULL,
                                                    method = c("splitconformal",
                                                               "jackknifeplus",
                                                               "other"),
                                                    ...) {
                                   method <- match.arg(method)
                                   super$predict(X = X, level = level,
                                                 method = method)
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
                                    if(is_package_available("xgboost") == FALSE)
                                      install.packages("xgboost",
                                                       repos = c(CRAN = "https://cloud.r-project.org"))
                                    list_params <- list(...)
                                    stopifnot(!is.null(list_params$params))
                                    stopifnot(!is.null(list_params$params$objective))
                                    stopifnot(sum(grepl(list_params$params$objective,
                                                        c("reg:logistic",
                                                          "binary:logistic",
                                                          "binary:logitraw",
                                                          "binary:hinge",
                                                          "multi:softmax",
                                                          "multi:softprob"))) >= 1)
                                    # following 3 instructions, in that order /!\
                                    private$encoded_factors <- encode_factors(as.factor(y))
                                    private$class_names <- as.character(levels(unique(as.factor(y))))
                                    y <- as.numeric(y) - 1
                                    self$X_train <- X
                                    self$y_train <- y
                                    self$params <- list(...)
                                    self$set_model(fit_func_xgboost(x = self$X_train,
                                                                    y = self$y_train,
                                                                    ...))
                                    self$set_engine(list(
                                      fit = function(x, y) fit_func_xgboost(x, y, ...),
                                      predict = function(obj, X) {
                                        return(predict(obj, X, reshape = TRUE))
                                      }
                                    ))
                                    return(base::invisible(self))
                                  },
                                  predict_proba = function(X, ...) {
                                    super$predict_proba(X = X, ...)
                                  },
                                  predict = function(X, ...) {
                                    super$predict(X = X, ...)
                                  }
                                ))

# predict_proba

# 3 - utils -------------------------------------------------------------------

fit_func_xgboost <- function(x, y, ...)
{
  xgboost::xgboost(data = x, label = y, ...)
}

