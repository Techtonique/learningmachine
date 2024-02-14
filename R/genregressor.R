

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
      type = "classification",
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
      set_regressor = function(regressor) {
        stopifnot(
          regressor %in% c(
            "bcn",
            "extratrees",
            "glmnet",
            "kernelridge",
            "ranger",
            "ridge",
            "xgboost"
          )
        )
        regressor <- match.arg(regressor)
        self$regressor <- regressor
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
      fit = function(X, y, ...) {
        stopifnot(is.factor(y))
        private$encoded_factors <- encode_factors(y)
        private$class_names <- as.character(levels(unique(y)))
        private$n_classes <- length(unique(y))
        private$y <- y
        Y <- one_hot(private$y)
        self$X_train <- X
        self$y_train <- Y
        self$params <- list(...)
        self$set_model(
          fit_regressor(
            x = self$X_train,
            y = private$y,
            regressor = self$regressor,
            show_progress = FALSE,
            ...
          )
        )
        self$set_engine(list(fit = fit_regressor,
                             predict = predict_regressor))
        return(base::invisible(self))
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
        if (is.null(self$engine) || is.null(self$model))
          stop(paste0(self$name, " must be fitted first"))
        if (is.null(level) && is.null(self$level))
        {
          stop("TODO")
        } else {
          # prediction sets with given 'level'
          if (is.null(self$level) && !is.null(level))
          {
            self$set_level(level)
          }
          if (!is.null(self$level) && !is.null(level))
          {
            if (self$level != level)
            {
              warning(paste0(
                "level parameter has been set to ",
                level,
                " instead of ",
                self$level
              ))
            }
            self$set_level(level)
          }
          stop("TODO")
        }
      }
    )
  )

# 2 - utils -------------------------------------------------------------------

fit_regressor <- function(x,
                          y,
                          regressor = c("bcn",
                                        "extratrees",
                                        "glmnet",
                                        "kernelridge",
                                        "ranger",
                                        "ridge",
                                        "xgboost"),
                          show_progress = FALSE,
                          ...) {
  n_classes <- length(unique(y))
  class_names <- as.character(levels(unique(y)))
  regressor <- match.arg(regressor)
  obj <- switch(
    regressor,
    bcn = function(x, y, ...) bcn::bcn(x, y, ...),
    extratrees = fit_func_extratrees_regression,
    glmnet = function(x, y, ...) glmnet::glmnet(x, y, ...),
    kernelridge = function(x, y, ...) fit_matern32_regression(x, y,
                                                              lambda = 0.1, 
                                                              ...),
    ranger = fit_func_ranger_regression,
    ridge = function(x, y, ...)
      fit_ridge_regression(x, y,
                           lambda = 10 ^ seq(-10, 10,
                                             length.out = 100), ...),
    xgboost = fit_func_xgboost
  )
  return(obj(x = x, y = y, ...))
}


predict_regressor <- function(obj,
                              X,
                              regressor = c("bcn",
                                            "extratrees",
                                            "glmnet",
                                            "kernelridge",
                                            "ranger",
                                            "ridge",
                                            "xgboost")) {
  
  regressor <- match.arg(regressor)
  
  predict_func <- switch(
    regressor,
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