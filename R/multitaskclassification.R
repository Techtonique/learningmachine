

# 1 - MultiTaskClassifier -----------------------------------------------------

MultiTaskClassifier <-
  R6::R6Class(
    classname = "MultiTaskClassifier",
    inherit = learningmachine::BaseClassifier,
    private = list(
      encoded_factors = NULL,
      class_names = NULL,
      n_classes = NULL,
      y = NULL
    ),
    public = list(
      name = "MultiTaskClassifier",
      type = "classification",
      model = NULL,
      regressor = "ranger",
      X_train = NULL,
      y_train = NULL,
      level = NULL,
      engine = NULL,
      params = NULL,
      seed = 123,
      initialize = function(name = "MultiTaskClassifier",
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
        stopifnot(regressor %in% c(
          "bcn",
          "extratrees",
          "glmnet",
          "kernelridge",
          "ranger",
          "ridge",
          "xgboost"
        ))
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
        self$set_model(fit_multitaskregressor(x = self$X_train,
                                              y = private$y,
                                              regressor = self$regressor,
                                              show_progress = FALSE,
                                              ...))
        self$set_engine(list(
          fit = fit_multitaskregressor,
          predict = predict_multitaskregressor
        ))
        return(base::invisible(self))
      },
      compute_probs = function(X) {
        if (is.null(self$engine) || is.null(self$model))
          stop(paste0(self$name, " must be fitted first"))
        raw_preds <- expit(self$engine$predict(self$model, X, regressor=self$regressor))
        probs <- raw_preds / rowSums(raw_preds)
        colnames(probs) <- private$class_names
        return(probs)
      },
      predict_proba = function(X,
                               level = NULL,
                               method = c(
                                 "kdesplitconformal",
                                 "kdejackknifeplus",
                                 "bootsplitconformal",
                                 "bootjackknifeplus",
                                 "surrsplitconformal",
                                 "surrjackknifeplus"
                               ),
                               B = 100,
                               ...) {
        method <- match.arg(method)
        if (is.null(level) && is.null(self$level))
        {
          return(self$compute_probs(X))
        }
        
        # prediction sets with given 'level'
        if (!is.null(self$level) &&
            !is.null(level) && self$level != level)
        {
          warning(paste0(
            "level parameter has been set to ",
            level,
            " instead of ",
            self$level
          ))
          self$set_level(level)
        }
        
        if (is.null(self$level) && !is.null(level))
        {
          self$set_level(level)
        }
        
        method <- match.arg(method)
        
        if (method %in% c("kdesplitconformal", 
                          "bootsplitconformal", 
                          "surrsplitconformal"))
        {
          idx_train_calibration <- split_data(private$y, p = 0.5,
                                              seed = self$seed)
          self$y_train <- one_hot(private$y)
          X_train_sc <- self$X_train[idx_train_calibration, ]
          y_train_sc <- self$y_train[idx_train_calibration, ]
          y_train_sc_factor <- private$y[idx_train_calibration]
          X_calibration_sc <- self$X_train[-idx_train_calibration, ]
          y_calibration_sc <- self$y_train[-idx_train_calibration, ]
          fit_objs_train_sc <- fit_multitaskregressor(x = X_train_sc,
                                                      y = y_train_sc_factor, 
                                                      regressor = self$regressor,
                                                      self$params)
          ################################################################ 'twice' but hard to circumvent
          self$set_model(fit_objs_train_sc)
          ################################################################
          y_pred_calibration <- self$engine$predict(self$model,
                                                    X = X_calibration_sc,
                                                    regressor = self$regressor)
          preds <- self$engine$predict(self$model, X, regressor = self$regressor)
          if(method == "kdesplitconformal")
          {
            stopifnot(!is.null(B) && B > 1)
            `%op%` <- foreach::`%do%`
            res <- foreach::foreach(i = 1:private$n_classes, 
                                    .verbose=FALSE) %op% {
            matrix_preds <- replicate(B, preds[, i])
            calibrated_raw_residuals <- y_calibration_sc[, i] - y_pred_calibration[, i]
            scaled_calibrated_residuals <- as.vector(base::scale(calibrated_raw_residuals,
                                                                 center = TRUE,
                                                                 scale = TRUE))
            sd_calibrated_residuals <- sd(calibrated_raw_residuals)
            simulated_scaled_calibrated_residuals <- rgaussiandens(x = scaled_calibrated_residuals,
                                                                   n = dim(preds)[1],
                                                                   p = B,
                                                                   seed = self$seed)
            sims <- matrix_preds + sd_calibrated_residuals * simulated_scaled_calibrated_residuals
            expit(sims)
          }
            
            if (!is.null(private$class_names))
            {
              names(res) <- private$class_names
            }
            
            list_probs <- compute_probs_list(res)
            
            return(compute_pis(list_probs, alpha = 1 - level / 100))
          } else {
            stop("Not implemented yet")
          }
          
        
      }
        },
      predict = function(X,
                         level = NULL,
                         method = c(
                           "kdesplitconformal",
                           "splitconformal",
                           "bootsplitconformal",
                           "bootjackknifeplus",
                           "surrsplitconformal",
                           "surrjackknifeplus"
                         ),
                         B = 100,
                         ...) {
        method <- match.arg(method)
        if (is.null(self$engine) || is.null(self$model))
          stop(paste0(self$name, " must be fitted first"))
        if (is.null(level) && is.null(self$level))
        {
          probs <- self$predict_proba(X)
          numeric_factor <- apply(probs, 1, which.max)
          res <- decode_factors(numeric_factor, private$encoded_factors)
          names(res) <- NULL
          return(res)
        } else {
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
          probs <- self$predict_proba(X,
                                      level = self$level,
                                      method = method,
                                      B = B)
          numeric_factor <- apply(probs$preds, 1, which.max)
          res <- decode_factors(numeric_factor, private$encoded_factors)
          names(res) <- NULL
          return(res)
        }
      }
    )
  )

# 2 - utils -------------------------------------------------------------------

fit_multitaskregressor <- function(x,
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
  Y <- as.matrix(one_hot(y))
  if (ncol(Y) != n_classes) 
    stop("The number classes in y must be equal to the number of classes")
  obj <- switch(
    regressor,
    bcn = bcn::bcn,
    extratrees = fit_func_extratrees_regression,
    glmnet = glmnet::glmnet,
    kernelridge = fit_matern32_regression,
    ranger = fit_func_ranger_regression,
    ridge = function(x, y, ...)
      fit_ridge_regression(x, y,
                           lambda = 10 ^ seq(-10, 10,
                                             length.out = 100), ...),
    xgboost = fit_func_xgboost
  )
  res <- vector("list", length = n_classes)
  names(res) <- class_names
  for (i in 1:n_classes) {
    res[[i]] <- obj(x, Y[, i], ...)
  }
  return(res)
}


predict_multitaskregressor <- function(objs,
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
  preds <- parfor(
    what = function(i)
      predict_func(objs[[i]], X),
    args = 1:length(objs),
    combine = cbind,
    show_progress = FALSE
  )
  return(preds)
}