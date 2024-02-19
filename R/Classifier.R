
# 1 - Class Classifier --------------------------------------------------------------

Classifier <-
  R6::R6Class(
    classname = "Classifier",
    inherit = learningmachine::Base,
    private = list(
      encoded_factors = NULL,
      class_names = NULL,
      n_classes = NULL,
      y = NULL,
      type_split = NULL,
      calib_resids = NULL,
      abs_calib_resids = NULL
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
      },
      fit = function(X,
                     y,
                     pi_method = c(
                       "splitconformal",
                       "kdesplitconformal",
                       "bootsplitconformal",
                       "jackknifeplus",
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
        private$encoded_factors <- encode_factors(y)
        private$class_names <- as.character(levels(unique(y)))
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
          self$set_model(fit_multitaskregressor(
            x = self$X_train,
            y = self$y_train,
            method = self$method,
            ...
          ))
          return()
        }
        
        if (self$pi_method %in% c("jackknifeplus", "kdejackknifeplus"))
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
        
        if (self$pi_method %in% c("splitconformal", "kdesplitconformal"))
        {
          idx_train_calibration <- split_data(
            self$y_train,
            p = 0.5,
            seed = self$seed,
            type_split = private$type_split
          )
          X_train_sc <-
            self$X_train[idx_train_calibration,]
          y_train_sc <-
            self$y_train[idx_train_calibration]
          X_calibration_sc <-
            self$X_train[-idx_train_calibration,]
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
              y_calibration_sc - y_pred_calibration
            private$abs_calib_resids <- abs(private$calib_resids)
            self$set_model(self$engine$fit(X_calibration_sc,
                                           y_calibration_sc))
          } else {
            self$set_model(fit_obj_train_sc)
            y_pred_calibration <-
              self$engine$predict(self$model,  # notice the diff
                                  X_calibration_sc)
            private$calib_resids <-
              y_calibration_sc - y_pred_calibration
            private$abs_calib_resids <- abs(private$calib_resids)
          }
        }
      },
      predict_proba = function(X,
                               level = NULL,
                               method = c(
                                 "kdesplitconformal",
                                 "kdejackknifeplus",
                                 "bootsplitconformal",
                                 "bootjackknifeplus"
                               ),
                               B = 100) {
        method <- match.arg(method)
        if (is.null(level) && is.null(self$level))
        {
          if (is.null(self$model) || is.null(self$engine))
            stop(paste0(self$name, " must be fitted first"))
          raw_preds <-
            expit(self$engine$predict(self$model, X))
          probs <-
            raw_preds / rowSums(raw_preds)
          colnames(probs) <-
            private$class_names
          return(probs)
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
                          "bootsplitconformal"))
        {
          idx_train_calibration <- split_data(private$y, p = 0.5,
                                              seed = self$seed)
          self$y_train <- one_hot(private$y)
          X_train_sc <- self$X_train[idx_train_calibration,]
          y_train_sc <- self$y_train[idx_train_calibration,]
          y_train_sc_factor <- private$y[idx_train_calibration]
          X_calibration_sc <- self$X_train[-idx_train_calibration,]
          y_calibration_sc <- self$y_train[-idx_train_calibration,]
          fit_objs_train_sc <-
            fit_multitaskregressor(
              x = as.matrix(X_train_sc),
              y = y_train_sc_factor,
              method = self$method,
              self$params
            )
          ################################################################ 'twice' but hard to circumvent
          self$set_model(fit_objs_train_sc)
          ################################################################
          y_pred_calibration <- self$engine$predict(self$model,
                                                    X = as.matrix(X_calibration_sc),
                                                    method = self$method)
          preds <- self$engine$predict(self$model, as.matrix(X),
                                       method = self$method)
          
          stopifnot(!is.null(B) && B > 1)
          `%op%` <- foreach::`%do%`
          
          # simulation of predictive probabilities
          res <- foreach::foreach(i = 1:private$n_classes,
                                  .verbose = FALSE) %op% {
                                    matrix_preds <- replicate(B, preds[, i])
                                    calibrated_raw_residuals <-
                                      y_calibration_sc[, i] - y_pred_calibration[, i]
                                    scaled_calibrated_residuals <-
                                      as.vector(base::scale(
                                        calibrated_raw_residuals,
                                        center = TRUE,
                                        scale = TRUE
                                      ))
                                    sd_calibrated_residuals <-
                                      sd(calibrated_raw_residuals)
                                    
                                    if (identical(method, "kdesplitconformal"))
                                    {
                                      simulated_scaled_calibrated_residuals <-
                                        suppressWarnings(
                                          rgaussiandens(
                                            x = scaled_calibrated_residuals,
                                            n = dim(preds)[1],
                                            p = B,
                                            seed = self$seed
                                          )
                                        )
                                    }
                                    
                                    if (identical(method, "bootsplitconformal")) {
                                      simulated_scaled_calibrated_residuals <-
                                        rbootstrap(
                                          x = scaled_calibrated_residuals,
                                          n = dim(preds)[1],
                                          p = B,
                                          seed = self$seed
                                        )
                                    }
                                    
                                    sims <-
                                      matrix_preds + sd_calibrated_residuals * simulated_scaled_calibrated_residuals
                                    expit(sims)
                                  }
          
          if (!is.null(private$class_names))
          {
            names(res) <- private$class_names
          }
          
          list_probs <-
            compute_probs_list(res)
          
          return(compute_pis(list_probs, alpha = 1 - level / 100))
          
        }
      },
      predict = function(X,
                         level = NULL,
                         method = c(
                           "kdesplitconformal",
                           "kdejackknifeplus",
                           "bootsplitconformal",
                           "bootjackknifeplus"
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
          res <-
            decode_factors(numeric_factor, private$encoded_factors)
          names(res) <- NULL
          return(res)
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
                                      B = B)
          numeric_factor <- apply(probs$preds, 1, which.max)
          res <-
            decode_factors(numeric_factor, private$encoded_factors)
          names(res) <- NULL
          return(res)
        }
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
                             pi_method = c(
                               "splitconformal",
                               "kdesplitconformal",
                               "bootsplitconformal",
                               "jackknifeplus",
                               "kdejackknifeplus",
                               "bootjackknifeplus",
                               "surrsplitconformal",
                               "surrjackknifeplus"
                             ),
                             B = 100,
                             seed = 123,
                             graph = FALSE,
                             ...) {
        stopifnot(pct_train >= 0.4 && pct_train < 1)
        stopifnot(length(y) == nrow(X))
        if (!is.null(level) && level != self$level)
        {
          warning(paste0(
            "attribute 'level' has been set to ",
            level,
            " instead of ",
            self$level
          ))
          self$level <- level
        }
        self$params <- list(...)
        pi_method <- match.arg(pi_method)
        set.seed(seed)
        train_index <-
          caret::createDataPartition(y, p = pct_train)$Resample1
        X_train <-
          as.matrix(X[train_index,])
        y_train <- y[train_index]
        X_test <-
          as.matrix(X[-train_index,])
        y_test <- y[-train_index]
        
        fit_obj <- self$fit(X_train, y_train,
                            pi_method = pi_method,
                            B = B, 
                            self$params)
        
        if (!is.null(self$level))
          # prediction intervals requested
        {
          res <- fit_obj$predict(X_test)
          
          if ((graph == TRUE) &&
              (!is.factor(y))) {
            y_values <- c(y_train, res$preds)
            y_upper <-
              c(y_train, res$upper)
            y_lower <-
              c(y_train, res$lower)
            x <- seq_along(y_values)
            xx <- c(x, rev(x))
            yy <-
              c(y_lower, rev(y_upper))
            plot(
              x,
              y_values,
              type = 'l',
              main = pi_method,
              xlab = "obs. index",
              ylab = "values",
              ylim = c(min(c(
                y_lower, y_upper, y_test
              )),
              max(c(
                y_lower, y_upper, y_test
              )))
            )
            polygon(xx, yy, col = "gray80", border = "gray80")
            lines(y_values, col = "red", lwd =
                    2)
            lines(c(y_train, y_test),
                  col = "black",
                  lwd = 2)
            lines(x[-seq_along(y_train)], y_test, col = "blue", lwd =
                    2)
          }
          
          # results ------
          results <- list()
          results$res <- res
          
          # point prediction
          results$score <-
            score(res$preds, y_test)
          
          # probabilistic prediction (can use res$lower, res$upper, and res$sims if method is kdejackknifeplus or kdesplitconformal)
          try_res <-
            try(score(res, y_test), silent = TRUE)
          if (!inherits(try_res, "try-error"))
          {
            results$score <- try_res
          }
          
          results$level <- level
          results$pi_method <- pi_method
          results$B <- B
          results$coverage <-
            mean(y_test >= res$lower & y_test <= res$upper) * 100
          results$length <-
            mean(res$upper - res$lower)
          
          return(results)
        } else {
          return(score(fit_obj$predict(X_test), y_test))
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
                                              "kernelridge",
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
    ranger = function(x, y, ...) fit_func_ranger_regression(x, y, ...),
    extratrees = function(x, y, ...) fit_func_extratrees_regression(x, y, ...),
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
                                                  "kernelridge",
                                                  "ranger",
                                                  "ridge", 
                                                  "xgboost")) {
  method <- match.arg(method)
  predict_func <- switch(
    method,
    bcn = bcn::predict.bcn,
    extratrees = predict_func_extratrees,
    glmnet = predict,
    kernelridge = predict_matern32,
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