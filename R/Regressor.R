


# 1 - Class Regressor --------------------------------------------------------------

Regressor <-
  R6::R6Class(
    classname = "Regressor",
    inherit = learningmachine::Base,
    private = list(
      y = NULL,
      type_split = NULL,
      calib_resids = NULL,
      abs_calib_resids = NULL
    ),
    public = list(
      name = "Regressor",
      type = "regression",
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
      initialize = function(name = "Regressor",
                            type = "regression",
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
        self$y_train <- drop(y)
        pi_method <- match.arg(pi_method)
        type_split <- match.arg(type_split)
        private$type_split <- type_split
        self$pi_method <- pi_method
        self$B <- B
        self$params <- list(...)
        self$set_engine(list(
          fit = function(x, y, ...)
            fit_regressor(x, y,
                          method = self$method,
                          ...),
          predict = function(obj, X, ...)
            predict_regressor(obj, X,
                              method = self$method,
                              ...)
        ))
        
        if (identical(self$pi_method, "none"))
        {
          self$set_model(fit_regressor(
            x = self$X_train,
            y = self$y_train,
            method = self$method,
            ...
          ))
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
                                              y_train_sc)
          
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
      predict = function(X,
                         level = NULL,
                         ...) {
        if (identical(self$pi_method, "none") &&
            (is.null(self$model) || is.null(self$engine)))
          stop(
            paste0(
              self$name,
              " must be fitted for method='",
              self$method,
              "' (use ",
              self$name,
              "$fit() first)"
            )
          )
        
        preds <- drop(self$engine$predict(self$model, X, ...))
        
        if (is.null(level) && is.null(self$level))
        {
          # no prediction interval
          return(preds)
        } else {
          # prediction intervals
          if (!is.null(self$level) &&
              !is.null(level) && self$level != level)
          {
            warning(paste0(
              "attribute 'level' has been set to ",
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
          
          if (self$pi_method %in% c("jackknifeplus", "kdejackknifeplus"))
          {
            if (identical(self$pi_method, "jackknifeplus"))
            {
              quantile_absolute_residuals <-
                quantile_scp(private$abs_calib_resids,
                             alpha = (1 - self$level / 100))
              return(
                list(
                  preds = preds,
                  lower = preds - quantile_absolute_residuals,
                  upper = preds + quantile_absolute_residuals
                )
              )
            }
            
            if (identical(self$pi_method, "kdejackknifeplus"))
            {
              scaled_raw_residuals <-
                scale(private$calib_resids,
                      center = TRUE,
                      scale = TRUE)
              sd_raw_residuals <- sd(private$calib_resids)
              simulated_raw_calibrated_residuals <-
                rgaussiandens(
                  private$calib_resids,
                  n = length(preds),
                  p = self$B,
                  seed = self$seed
                )
              sims <-
                replicate(self$B, preds) + sd_raw_residuals * simulated_raw_calibrated_residuals
              preds_lower <-
                apply(sims, 1, function(x)
                  quantile(x, probs = (1 - self$level / 100) / 2))
              preds_upper <-
                apply(sims, 1, function(x)
                  quantile(x, probs = 1 - (1 - self$level / 100) / 2))
              return(list(
                preds = apply(sims, 1, mean),
                sims = sims,
                lower = preds_lower,
                upper = preds_upper
              ))
            }
          }
          
          if (self$pi_method %in% c("splitconformal", "kdesplitconformal"))
          {
            if (identical(self$pi_method, "splitconformal"))
            {
              quantile_absolute_residuals <-
                quantile_scp(private$abs_calib_resids,
                             alpha = (1 - self$level / 100))
              return(
                list(
                  preds = preds,
                  lower = preds - quantile_absolute_residuals,
                  upper = preds + quantile_absolute_residuals
                )
              )
            }
            
            if (identical(self$pi_method, "kdesplitconformal"))
            {
              scaled_calibrated_residuals <-
                base::scale(private$calib_resids,
                            center = TRUE,
                            scale = TRUE)
              sd_calibrated_residuals <- sd(private$calib_resids)
              simulated_scaled_calibrated_residuals <-
                rgaussiandens(
                  scaled_calibrated_residuals,
                  n =
                    length(preds),
                  p =
                    self$B,
                  seed =
                    self$seed
                )
              matrix_preds <- replicate(self$B, preds)
              sims <-
                matrix_preds + sd_calibrated_residuals * simulated_scaled_calibrated_residuals
              preds_lower <-
                apply(sims, 1, function(x)
                  quantile(x, probs = (1 - self$level / 100) / 2))
              preds_upper <-
                apply(sims, 1, function(x)
                  quantile(x, probs = 1 - (1 - self$level / 100) / 2))
              return(list(
                preds = apply(sims, 1, mean),
                sims = sims,
                lower = preds_lower,
                upper = preds_upper
              ))
            }
            
          }
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
                             graph = FALSE) {
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
                            B = B)
        
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
          results$winkler_score <-
            winkler_score(obj = res,
                          actual = y_test,
                          level = self$level)
          results$length <-
            mean(res$upper - res$lower)
          
          return(results)
        } else {
          return(score(fit_obj$predict(X_test), y_test))
        }
        
      },
      summary = function(X,
                         level = 95,
                         show_progress = TRUE,
                         y = NULL,
                         cl = NULL) {
        if (is.null(self$engine) || is.null(self$model))
          stop(paste0(self$name, " must be fitted first (use ", self$name, "$fit())"))
        
        if (is_package_available("skimr") == FALSE)
        {
          utils::install.packages("skimr")
        }
        
        deriv_column <- function(ix)
        {
          zero <- 1e-4
          eps_factor <- zero ^ (1 / 3)
          X_plus <- X
          X_minus <- X
          X_ix <- X[, ix]
          cond <- abs(X_ix) > zero
          h <- eps_factor * X_ix * cond + zero * (!cond)
          X_plus[, ix] <- X_ix + h
          X_minus[, ix] <- X_ix - h
          derived_column <-
            try((self$predict(as.matrix(X_plus)) - self$predict(as.matrix(X_minus))) /
                  (2 * h), silent = TRUE)
          if (inherits(derived_column, "try-error"))
            derived_column <-
            (self$predict(as.matrix(X_plus))$preds - self$predict(as.matrix(X_minus))$preds) /
            (2 * h)
          return (derived_column)
        }
        #deriv_column <- compiler::cmpfun(deriv_column)
        
        effects <- parfor(
          what = deriv_column,
          args = seq_len(ncol(X)),
          show_progress = show_progress,
          verbose = FALSE,
          combine = cbind,
          cl = cl
        )
        
        names_X <- colnames(X)
        if (!is.null(names_X))
        {
          colnames(effects) <- names_X
        } else {
          colnames(effects) <- paste0("V", seq_len(ncol(X)))
        }
        
        foo_tests <- function(x)
        {
          res <- stats::t.test(x)
          return(c(
            as.numeric(res$estimate),
            as.numeric(res$conf.int),
            res$p.value
          ))
        }
        
        lower_signif_codes <- c(0, 0.001, 0.01, 0.05, 0.1)
        upper_signif_codes <- c(0.001, 0.01, 0.05, 0.1, 1)
        signif_codes <- c("***", "**", "*", ".", "")
        choice_signif_code <-
          function(x)
            signif_codes[which.max((lower_signif_codes <= x) * (upper_signif_codes > x))]
        ttests <-
          try(data.frame(t(apply(effects, 2, foo_tests))), silent = TRUE)
        if (!inherits(ttests, "try-error"))
        {
          colnames(ttests) <- c("estimate", "lower", "upper", "p-value")
          ttests$signif <-
            sapply(ttests[, 3], choice_signif_code) # p-values signif.
          if (!is.null(y))
          {
            preds <- self$predict(as.matrix(X))
            y_hat <- try(preds$preds,
                         silent = TRUE)
            if (inherits(y_hat, "try-error"))
              y_hat <- self$predict(as.matrix(X))
            Residuals <- y - y_hat
            R_squared <-
              1 - sum((y - y_hat) ^ 2) / sum((y - mean(y)) ^ 2)
            if (!is.null(self$level))
            {
              winkler_score_ <- winkler_score(obj = preds,
                                              actual = y,
                                              level = self$level)
              coverage_rate <-
                mean(y >= preds$lower & y <= preds$upper) * 100
            } else {
              winkler_score_ <- NULL
              coverage_rate <- NULL
            }
            return(
              list(
                R_squared = R_squared,
                Residuals = summary(Residuals),
                Winkler_score = winkler_score_,
                Coverage_rate = coverage_rate,
                ttests = ttests,
                effects = my_skim(effects)
              )
            )
          } else {
            return(list(ttests = ttests,
                        effects = my_skim(effects)))
          }
        } else {
          return(my_skim(effects))
        }
        
        
      }
    )
  )

# 2 - utils -------------------------------------------------------------------

fit_regressor <- function(x,
                          y,
                          method = c("lm",
                                     "ranger",
                                     "extratrees",
                                     "ridge",
                                     "bcn",
                                     "glmnet",
                                     "krr"),
                          ...) {
  regressor_choice <- match.arg(method)
  obj <- switch(
    regressor_choice,
    lm = function(x, y)
      stats::.lm.fit(x, y),
    ranger = fit_func_ranger_regression,
    extratrees = fit_func_extratrees_regression,
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
                              ...)
  )
  return(obj(x = x, y = y, ...))
}


predict_regressor <- function(obj,
                              X,
                              method = c("lm",
                                         "ranger",
                                         "extratrees",
                                         "ridge",
                                         "bcn",
                                         "glmnet",
                                         "krr",
                                         "xgboost")) {
  method_choice <- match.arg(method)
  
  predict_func <- switch(
    method_choice,
    lm = function(object, X)
      X %*% object$coefficients,
    bcn = bcn::predict.bcn,
    extratrees = predict_func_extratrees,
    glmnet = predict,
    krr = predict_matern32,
    ranger = predict_func_ranger,
    ridge = predict_ridge_regression,
    xgboost = predict
  )
  return(predict_func(obj, X))
}