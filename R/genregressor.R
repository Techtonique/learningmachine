




# 1 - Regressor -----------------------------------------------------

Regressor <-
  R6::R6Class(
    classname = "Regressor",
    inherit = learningmachine::BaseRegressor,
    private = list(
      encoded_factors = NULL,
      class_names = NULL,
      n_classes = NULL,
      y = NULL
    ),
    public = list(
      name = "Regressor",
      type = "regression",
      model = NULL,
      regressor = "ranger",
      X_train = NULL,
      y_train = NULL,
      level = NULL,
      engine = NULL,
      params = NULL,
      seed = 123,
      initialize = function(name = "Regressor",
                            type = "classification",
                            model = NULL,
                            regressor = "ranger",
                            X_train = NULL,
                            y_train = NULL,
                            level = NULL,
                            engine = NULL,
                            params = NULL,
                            seed = 123) {
        self$name <- name
        self$type <- type
        self$model <- model
        self$regressor <- regressor
        self$X_train <- X_train
        self$y_train <- y_train
        self$level <- level
        self$engine <- engine
        self$params <- params
        self$seed <- seed
      },
      get_name = function() {
        return(self$name)
      },
      get_type = function() {
        return(self$type)
      },
      get_model = function() {
        self$model
      },
      set_model = function(model) {
        self$model <- model
      },
      get_regressor = function() {
        self$regressor
      },
      set_regressor = function(regressor = c("ranger",
                                             "extratrees",
                                             "ridge",
                                             "bcn",
                                             "glmnet",
                                             "kernelridge",
                                             "xgboost")) {
        regressor_choice <- match.arg(regressor)
        stopifnot(
          regressor_choice %in% c(
            "ranger",
            "extratrees",
            "ridge",
            "bcn",
            "glmnet",
            "kernelridge",
            "xgboost"
          )
        )
        self$regressor <- regressor_choice
      },
      get_level = function() {
        self$level
      },
      set_level = function(level) {
        self$level <- level
      },
      set_engine = function(engine) {
        self$engine <- engine
      },
      get_engine = function() {
        self$engine
      },
      get_params = function() {
        self$params
      },
      get_seed = function() {
        self$seed
      },
      set_seed = function(seed) {
        self$seed <- seed
      },
      fit = function(X,
                     y,
                     ...) {
        self$X_train <- X
        self$y_train <- y
        self$params <- list(...)
        
        self$set_model(fit_regressor(self$X_train,
                                     self$y_train,
                                     regressor = self$regressor,
                                     ...))
        self$set_engine(list(
          fit = function(x, y, ...) fit_regressor(x, y, 
                                                  regressor = self$regressor, 
                                                  ...),
          predict = function(obj, X, ...)
            predict_regressor(obj, X,
                              regressor = self$regressor, 
                              ...)
        ))
      },
      predict = function(X,
                         level = NULL,
                         method = c(
                           "kdesplitconformal",
                           "kdejackknifeplus",
                           "bootsplitconformal",
                           "bootjackknifeplus"
                         ),
                         B = 250,
                         ...) {
        method <- match.arg(method)
        super$predict(
          X = X,
          level = level,
          method = method,
          B = B
        )
      },
      fit_predict = function(X,
                             y,
                             pct_train = 0.8,
                             score = ifelse(
                               is.factor(y),
                               yes = function(preds, y_test)
                                 mean(preds == y_test),
                               no = function(preds, y_test)
                                 sqrt(mean((preds - y_test) ^ 2))
                             ),
                             level = NULL,
                             method = c("splitconformal",
                                        "jackknifeplus",
                                        "kdesplitconformal",
                                        "kdejackknifeplus"),
                             B = 100,
                             seed = 123,
                             graph = FALSE) {
        method <- match.arg(method)
        super$fit_predict(
          X = X,
          y = y,
          pct_train = pct_train,
          score = score,
          level = level,
          method = method,
          B = B,
          seed = seed,
          graph = graph
        )
      }
    )
  )

# 2 - utils -------------------------------------------------------------------

fit_regressor <- function(x,
                          y,
                          regressor = c("ranger",
                                        "extratrees",
                                        "ridge",
                                        "bcn",
                                        "glmnet",
                                        "kernelridge",
                                        "xgboost"),
                          ...) {
  n_classes <- length(unique(y))
  class_names <- as.character(levels(unique(y)))
  regressor_choice <- match.arg(regressor)
  obj <- switch(
    regressor_choice,
    bcn = function(x, y, ...)
      bcn::bcn(x, y, ...),
    extratrees = fit_func_extratrees_regression,
    glmnet = function(x, y, ...)
      glmnet::glmnet(x, y, ...),
    kernelridge = function(x, y, ...)
      fit_matern32_regression(x, y,
                              lambda = 0.1,
                              centering = TRUE,
                              ...),
    ranger = fit_func_ranger_regression,
    ridge = function(x, y, ...)
      fit_ridge_regression(x, y,
                           lambda = 10 ^ seq(-10, 10,
                                             length.out = 100), ...),
    xgboost = function(x, y, ...) fit_func_xgboost(x, y, ...)
  )
  return(obj(x = x, y = y, ...))
}


predict_regressor <- function(obj,
                              X,
                              regressor = c("ranger",
                                            "extratrees",
                                            "ridge",
                                            "bcn",
                                            "glmnet",
                                            "kernelridge",
                                            "xgboost")) {
  regressor_choice <- match.arg(regressor)
  predict_func <- switch(
    regressor_choice,
    bcn = bcn::predict.bcn,
    extratrees = predict_func_extratrees,
    glmnet = predict,
    kernelridge = predict_matern32,
    ranger = predict_func_ranger,
    ridge = predict_ridge_regression,
    xgboost = predict
  )
  return(predict_func(obj, X))
}