
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
                                   self$set_engine(fit_func_xgboost)
                                   self$set_model(fit_func_xgboost(x = X, y = y,
                                                                   ...))
                                   return(base::invisible(self))
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
                                    self$set_engine(fit_func_xgboost)
                                    self$set_model(fit_func_xgboost(x = X, y = y,
                                                                    ...))
                                    return(base::invisible(self))
                                  },
                                  predict = function(X, ...) {
                                    predict(self$model, X, ...)
                                  }
                                ))

# predict_proba

# 3 - utils -------------------------------------------------------------------

fit_func_xgboost <- function(x, y, ...)
{
  xgboost::xgboost(data = x, label = y, ...)
}

