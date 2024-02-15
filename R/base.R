
BaseClassifier <- R6::R6Class(
  classname = "BaseClassifier",
  private = list(
    encoded_factors = NULL,
    class_names = NULL,
    n_classes = NULL,
    y = NULL
  ),
  public = list(
    name = "BaseClassifier",
    type = "classification",
    model = NULL,
    X_train = NULL,
    y_train = NULL,
    level = NULL,
    engine = NULL,
    params = NULL,
    seed = 123,
    initialize = function(name = "BaseClassifier",
                          type = "classification",
                          model = NULL,
                          X_train = NULL,
                          y_train = NULL,
                          level = NULL,
                          engine = NULL,
                          params = NULL,
                          seed = 123) {
      self$name <- name
      self$type <- type
      self$model <- model
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
      private$encoded_factors <-
        encode_factors(y)
      private$class_names <-
        as.character(levels(unique(y)))
      private$n_classes <- length(unique(y))
      private$y <- y
      Y <- one_hot(private$y)
      self$X_train <- X
      self$y_train <- Y
      self$params <- list(...)
      self$set_model(stats::.lm.fit(x = self$X_train,
                                    y = self$y_train,
                                    ...))
      self$set_engine(list(
        fit = stats::.lm.fit,
        predict = function(obj, X)
          drop(X %*% obj$coefficients)
      ))
      return(base::invisible(self))
    },
    compute_probs = function(X, ...) {
      if (is.null(self$engine) || is.null(self$model))
        stop(paste0(self$name, " must be fitted first"))
      raw_preds <- expit(self$engine$predict(self$model, X))
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
      if (is.null(self$engine) || is.null(self$model))
        stop(paste0(self$name, " must be fitted first"))
      if (is.null(level) && is.null(self$level))
      {
        return(self$compute_probs(X, ...))
      } else {
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
        
        if (identical(method, "kdesplitconformal"))
        {
          idx_train_calibration <- split_data(private$y, p = 0.5,
                                              seed = self$seed)
          self$y_train <- one_hot(private$y)
          X_train_sc <-
            as.matrix(self$X_train)[idx_train_calibration, ]
          y_train_sc <-
            try(as.matrix(self$y_train)[idx_train_calibration, ],
                silent = TRUE)
          if (try(inherits(y_train_sc, "try-error"), silent = TRUE)
          )
          {
            y_train_sc <- self$y_train[idx_train_calibration]
          }
          X_calibration_sc <- self$X_train[-idx_train_calibration, ]
          y_calibration_sc <-
            try(self$y_train[-idx_train_calibration, ],
                silent = TRUE)
          if (try(inherits(y_calibration_sc, "try-error"), silent = TRUE)
          )
          {
            y_calibration_sc <- self$y_train[-idx_train_calibration]
          }
          fit_objs_train_sc <- vector("list",
                                      private$n_classes)
          for (i in seq_len(private$n_classes))
          {
            fit_objs_train_sc[[i]] <- self$engine$fit(as.matrix(X_train_sc),
                                                      factor(y_train_sc[, i]))
          }
          
          ################################################################ 'twice' but hard to circumvent
          self$set_model(fit_objs_train_sc)
          ################################################################
          
          y_pred_calibration <-
            parfor(
              what = function(i)
                self$engine$predict(self$model[[i]],
                                    as.matrix(X_calibration_sc)),
              args = seq_len(private$n_classes),
              combine = cbind,
              show_progress = FALSE
            )
          #abs_residuals <-
          #  abs(y_calibration_sc - y_pred_calibration)
          preds <-
            parfor(
              what = function(i)
                self$engine$predict(self$model[[i]],
                                    as.matrix(X),
                                    ...),
              args = seq_len(private$n_classes),
              combine = cbind,
              show_progress = FALSE
            )
          
          if (identical(method, "kdesplitconformal"))
          {
            stopifnot(!is.null(B) && B > 1)
            
            `%op%` <- foreach::`%do%`
            
            res <- foreach::foreach(i = seq_len(private$n_classes), 
                                    .verbose = FALSE) %op% {
              matrix_preds <- replicate(B, as.vector(preds[, i]))
              calibrated_raw_residuals <-
                as.vector(y_calibration_sc[, i]) - as.vector(y_pred_calibration[, i])
              scaled_calibrated_residuals <- as.vector(base::scale(calibrated_raw_residuals,
                            center = TRUE,
                            scale = TRUE))
              sd_calibrated_residuals <- sd(calibrated_raw_residuals)
              simulated_scaled_calibrated_residuals <-
                rgaussiandens(
                  x = scaled_calibrated_residuals,
                  n = dim(preds)[1],
                  p = B,
                  seed = self$seed
                )
              sims <- matrix_preds + sd_calibrated_residuals * simulated_scaled_calibrated_residuals
              expit(sims)
              }
            
            if (!is.null(private$class_names))
            {
              names(res) <- private$class_names
            }
            
            list_probs <- compute_probs_list(res)
            
            return(compute_pis(list_probs, alpha = 1 - level / 100))
          }
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
      if (is.null(level) && is.null(self$level))
      {
        probs <- self$predict_proba(X,
                                    ...)
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
        
        probs <- self$predict_proba(X,
                                    level = self$level,
                                    method = method,
                                    B = B,
                                    ...)$preds
        
      }
      numeric_factor <- apply(probs, 1, which.max)
      res <- decode_factors(numeric_factor, private$encoded_factors)
      names(res) <- NULL
      return(res)
    }
  )
)
