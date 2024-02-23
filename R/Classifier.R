
# 1 - Class Classifier --------------------------------------------------------------

Classifier <-
  R6::R6Class(
    classname = "Classifier",
    inherit = learningmachine::Base,
    private = list(
      y = NULL, 
      encoded_factors = NULL,
      class_names = NULL,
      n_classes = NULL,
      type_split = NULL,
      calib_resids = NULL,
      abs_calib_resids = NULL,
      q_threshold = NULL
    ),
    public = list(
      name = "Classifier",
      type = "classification",
      model = NULL,
      method = NULL,
      X_train = NULL,
      y_train = NULL,
      pi_method = NULL,
      level = NULL,
      type_prediction_set = "score",
      B = NULL,
      engine = NULL,
      params = NULL,
      seed = 123,
      initialize = function(name = "Classifier",
                            type = "classification",
                            model = NULL,
                            method = NULL,
                            X_train = NULL,
                            y_train = NULL,
                            pi_method = NULL,
                            level = NULL,
                            type_prediction_set = "score",
                            B = NULL,
                            engine = NULL,
                            params = NULL,
                            seed = 123) {
        super$initialize(
          name = name,
          type = type,
          model = model,
          method = method,
          X_train = X_train,
          y_train = y_train,
          pi_method = pi_method,
          level = level,
          B = B,
          engine = engine,
          params = params,
          seed = seed
        )
        self$type_prediction_set <- type_prediction_set
      },
      get_type_prediction_set = function() {
        self$type_prediction_set
      },
      set_type_prediction_set = function(type_prediction_set) {
        self$type_prediction_set <- type_prediction_set
      },
      fit = function(X,
                     y,
                     pi_method = c(
                       "kdesplitconformal",
                       "bootsplitconformal",
                       "kdejackknifeplus",
                       "bootjackknifeplus",
                       "surrsplitconformal",
                       "surrjackknifeplus"
                     ),
                     type_split = c("stratify",
                                    "sequential"),
                     B = 100,
                     ...) {
        self$X_train <- X
        self$y_train <- y
        private$y <- y
        private$encoded_factors <- encode_factors(private$y)
        private$class_names <- as.character(levels(unique(private$y)))
        private$n_classes <- length(private$class_names)
        pi_method <- match.arg(pi_method)
        type_split <- match.arg(type_split)
        private$type_split <- type_split
        self$pi_method <- pi_method
        self$B <- B
        self$params <- list(...)
        self$set_engine(
          list(
            fit = function(x, y, ...)
              fit_multitaskregressor(x, y,
                                     method = self$method,
                                     ...),
            predict = function(obj, X)
              predict_multitaskregressor(obj, X,
                                         method = self$method)
          )
        )
        
        if (is.null(self$level))
        {
          self$set_model(
            fit_multitaskregressor(
              x = self$X_train,
              y = private$y,
              method = self$method,
              ...
            )
          )
          return (invisible(self))
        }
        
        if (self$pi_method %in% c("kdejackknifeplus",
                                  "bootjackknifeplus",
                                  "surrjackknifeplus"))
        {
          n_train <- nrow(self$X_train)
          fit_objs_loocv <-
            base::vector("list", n_train)
          abs_residuals_loocv <-
            rep(0, n_train)
          raw_residuals_loocv <-
            rep(0, n_train)
          
          # sequential execution of the jackknife procedure
          if (self$cl %in% c(0, 1) ||
              is.null(self$cl))
          {
            pb <- txtProgressBar(min = 1,
                                 max = n_train,
                                 style = 3)
            for (i in 1:n_train) {
              left_out_indices <- setdiff(1:n_train, i)
              obj_jackknife_residuals <-
                get_jackknife_residuals(
                  X = self$X_train,
                  y = self$y_train,
                  idx = left_out_indices,
                  fit_func = self$engine$fit,
                  predict_func = self$engine$predict
                )
              abs_residuals_loocv[i] <-
                obj_jackknife_residuals$abs_residuals
              raw_residuals_loocv[i] <-
                obj_jackknife_residuals$raw_residuals
              setTxtProgressBar(pb, i)
            }
            close(pb)
            private$abs_calib_resids <- abs_residuals_loocv
            private$calib_resids <- raw_residuals_loocv
          } else {
            # self$cl > 1 # parallel execution of the jackknife procedure
            loofunc <- function(idx) {
              left_out_indices <- base::setdiff(1:n_train, idx)
              obj_jackknife_residuals <-
                get_jackknife_residuals(
                  X = self$X_train,
                  y = self$y_train,
                  idx = left_out_indices,
                  fit_func = self$engine$fit,
                  predict_func = self$engine$predict
                )
              return(
                c(
                  obj_jackknife_residuals$abs_residuals #/!\ keep, part of loofunc /!\
                  ,
                  obj_jackknife_residuals$raw_residuals #/!\ keep, part of loofunc /!\
                )
              )
            }
            
            if (is_package_available("doSNOW") == FALSE)
            {
              utils::install.packages("doSNOW", repos = "https://cran.rstudio.com/")
            }
            
            residuals_init <- rep(0, 2 * n_train)
            residuals_vec <- parfor(what = loofunc,
                                    args = seq_len(n_train),
                                    cl = self$cl)
            residuals_matrix <- matrix(residuals_vec,
                                       nrow = n_train,
                                       ncol = 2,
                                       byrow = TRUE)
            private$abs_calib_resids <- residuals_matrix[, 1]
            private$calib_resids <- residuals_matrix[, 2]
          }
        }
        
        if (self$pi_method %in% c(
          "kdesplitconformal",
          "bootsplitconformal",
          "surrsplitconformal"
        ))
        {
          idx_train_calibration <- split_data(
            private$y,
            p = 0.5,
            seed = self$seed,
            type_split = private$type_split
          )
          X_train_sc <-
            self$X_train[idx_train_calibration, ]
          y_train_sc <-
            self$y_train[idx_train_calibration]
          X_calibration_sc <-
            self$X_train[-idx_train_calibration, ]
          y_calibration_sc <-
            self$y_train[-idx_train_calibration]
          
          fit_obj_train_sc <- self$engine$fit(X_train_sc,
                                              y_train_sc,
                                              ...)
          
          if (private$type_split == "sequential")
          {
            y_pred_calibration <-
              self$engine$predict(fit_obj_train_sc, # notice the diff
                                  X_calibration_sc)
            private$calib_resids <-
              one_hot(y_calibration_sc) - y_pred_calibration
            private$abs_calib_resids <- abs(private$calib_resids)
            self$set_model(self$engine$fit(X_calibration_sc,
                                           y_calibration_sc))
          } else {
            self$set_model(fit_obj_train_sc)
            y_pred_calibration <-
              self$engine$predict(self$model,  # notice the diff
                                  X_calibration_sc)
            private$calib_resids <-
              one_hot(y_calibration_sc) - y_pred_calibration
            private$abs_calib_resids <- abs(private$calib_resids)
          }
          private$q_threshold <- get_threshold(
              probs = self$predict_proba(X_calibration_sc),
              y_calibration_sc = one_hot(y_calibration_sc),
              level = self$level
            )
        }
        
        return(invisible(self))
      },
      predict_proba = function(X) {
          if (is.null(self$model) || is.null(self$engine))
            stop(paste0(self$name, " must be fitted first"))
          raw_preds <-
            expit(self$engine$predict(self$model, X))
          probs <-
            raw_preds / rowSums(raw_preds)
          colnames(probs) <-
            private$class_names
          return(probs)
      },
      predict = function(X,
                         level = NULL) {
        probs <- self$predict_proba(X)
        if (is.null(level) && is.null(self$level))
        {
          numeric_factor <- apply(probs, 1, which.max)
          res <- decode_factors(numeric_factor, 
                                private$encoded_factors)
          names(res) <- NULL
          return(res)
          
        } else { # !is.null(level) || !is.null(self$level)
          
          res <- list()
          res$preds <- NULL # predictions sets with given 'level'
          res$lower <- NULL # upon request
          res$upper <- NULL # upon request
          res$sims <- NULL # upon request
          
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
          
          if (self$type_prediction_set == "score")
          {
            ix_list <- get_classes_idx(probs, 
                                  private$q_threshold, 
                                  self$level)
            list_classes <- decode_factors2(ix_list)
            res$preds <- impute_classes(list_classes, probs)
            return(res)
          }
        }
      }
    )
  )


# 2 - utils -------------------------------------------------------------------

fit_multitaskregressor <- function(x,
                                   y,
                                   method = c("bcn",
                                              "extratrees",
                                              "glmnet",
                                              "krr",
                                              "ranger",
                                              "ridge",
                                              "xgboost"),
                                   show_progress = FALSE,
                                   ...) {
  n_classes <- length(unique(y))
  class_names <- as.character(levels(unique(y)))
  regressor_choice <- match.arg(method)
  Y <- as.matrix(one_hot(y))
  if (ncol(Y) != n_classes)
    stop("The number classes in y must be equal to the number of classes")
  obj <- switch(
    regressor_choice,
    lm = function(x, y, ...)
      stats::.lm.fit(x, y),
    ranger = function(x, y, ...)
      fit_func_ranger_regression(x, y, ...),
    extratrees = function(x, y, ...)
      fit_func_extratrees_regression(x, y, ...),
    ridge = function(x, y, ...)
      fit_ridge_regression(x, y,
                           lambda = 10 ^ seq(-10, 10,
                                             length.out = 100), ...),
    bcn = function(x, y, ...)
      bcn::bcn(x, y, ...),
    glmnet = function(x, y, ...)
      glmnet::glmnet(x, y, ...),
    krr = function(x, y, ...)
      fit_matern32_regression(x, y,
                              ...),
    xgboost = function(x, y, ...)
      fit_xgboost_regression(x, y, ...)
  )
  res <- vector("list", length = n_classes)
  names(res) <- class_names
  for (i in 1:n_classes) {
    res[[i]] <- obj(x = x, y = Y[, i], ...)
  }
  return(res)
}


predict_multitaskregressor <- function(objs,
                                       X,
                                       method = c("bcn",
                                                  "extratrees",
                                                  "glmnet",
                                                  "krr",
                                                  "ranger",
                                                  "ridge",
                                                  "xgboost")) {
  method <- match.arg(method)
  predict_func <- switch(
    method,
    bcn = bcn::predict.bcn,
    extratrees = predict_func_extratrees,
    glmnet = predict,
    krr = predict_matern32,
    ranger = predict_func_ranger,
    ridge = predict_ridge_regression,
    xgboost = predict
  )
  preds <- matrix(NA, nrow = nrow(X),
                  ncol = length(objs))
  for (i in 1:length(objs)) {
    preds[, i] <- predict_func(objs[[i]], X)
  }
  return(preds)
}