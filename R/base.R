


# 1 - BaseRegressor -----------------------------------------------------------

BaseRegressor <- R6::R6Class(
  "BaseRegressor",
  public = list(
    name = "BaseRegressor",
    type = "regression",
    model = NULL,
    X_train = NULL,
    y_train = NULL,
    level = NULL, 
    engine = NULL,
    params = NULL,
    cl = NULL,
    seed = 123,
    initialize = function(name = "BaseRegressor",
                          type = "regression",
                          model = NULL,
                          X_train = NULL,
                          y_train = NULL,
                          level = NULL, 
                          engine = NULL,
                          params = NULL,
                          cl = NULL,
                          seed = 123) {
      self$name <- name
      self$type <- type
      self$model <- model
      self$X_train <- X_train
      self$y_train <- y_train
      self$level <- level
      self$engine <- engine
      self$params <- params
      self$cl <- cl
      self$seed <-
        seed
    },
    get_name = function() {
      self$name
    },
    get_type = function() {
      self$type
    },
    get_model = function() {
      self$model
    },
    set_model = function(model) {
      self$model <- model
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
    get_cl = function() {
      self$cl
    },
    set_cl = function(cl) {
      self$cl <- cl
    },
    set_seed = function(seed) {
      self$seed <- seed
    },
    get_level = function() {
      self$level
    },
    set_level = function(level) {
      self$level <- level
    },
    fit = function(X, y, ...) {
      self$X_train <- X
      self$y_train <- y
      self$params <- list(...)
      
      self$set_model(stats::.lm.fit(x = self$X_train,
                                    y = self$y_train,
                                    ...))
      self$set_engine(list(
        fit = stats::.lm.fit,
        predict = function(obj, X){
          drop(X %*% obj$coefficients)
        }
          
      ))
      return(base::invisible(self))
    },
    predict = function(X,
                       level = NULL,
                       method = c("splitconformal",
                                  "jackknifeplus",
                                  "kdesplitconformal",
                                  "kdejackknifeplus"),
                       B = 100,
                       ...) {
      if (is.null(self$engine))
        stop(paste0(self$name, " must be fitted first (use ", self$name, "$fit())"))
      
      if (is.null(level) && is.null(self$level)) # no prediction interval
      {
        
        return(self$engine$predict(self$model, X))
        
      } else { # prediction intervals
        
        if(!is.null(self$level) && !is.null(level) && self$level != level)
        {
          warning(paste0("level parameter has been set to ", level, " instead of ", self$level))
          self$set_level(level)
        }
        
        if (is.null(self$level) && !is.null(level))
        {
          self$set_level(level)  
        }
        
        method <- match.arg(method)
        
        if (method %in% c("jackknifeplus", "kdejackknifeplus"))
        {
          stopifnot(!is.null(self$X_train))
          stopifnot(!is.null(self$y_train))
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
                  obj_jackknife_residuals$abs_residuals
                  ,
                  obj_jackknife_residuals$raw_residuals
                )
              )
            }
            
            if (is_package_available("doSNOW") == FALSE)
            {
              utils::install.packages("doSNOW", repos = "https://cran.rstudio.com/")
            }
            
            residuals_init <- rep(0, 2*n_train)
            residuals_vec <- parfor(what = loofunc,
                                    args = seq_len(n_train),
                                    cl = self$cl)
            residuals_matrix <- matrix(
                residuals_vec,
                nrow = n_train,
                ncol = 2,
                byrow = TRUE
              )
            abs_residuals_loocv <- residuals_matrix[, 1]
            raw_residuals_loocv <- residuals_matrix[, 2]
          }
          
          preds <- self$engine$predict(self$model, 
                                       X, ...) #/!\ keep
          
          if (identical(method, "jackknifeplus"))
          {
            quantile_absolute_residuals <- quantile_scp(abs_residuals_loocv,
                                                        alpha = (1 - self$level / 100))
            return(
              list(
                preds = preds,
                lower = preds - quantile_absolute_residuals,
                upper = preds + quantile_absolute_residuals
              )
            )
          }
          
          if (identical(method, "kdejackknifeplus"))
          {
            stopifnot(!is.null(B) && B > 1)
            scaled_raw_residuals <-
              scale(raw_residuals_loocv,
                    center = TRUE,
                    scale = TRUE)
            sd_raw_residuals <-
              sd(raw_residuals_loocv)
            simulated_raw_calibrated_residuals <-
              rgaussiandens(
                scaled_raw_residuals,
                n =
                  length(preds),
                p =
                  B,
                seed =
                  self$seed
              )
            sims <-
              replicate(B, preds) + sd_raw_residuals * simulated_raw_calibrated_residuals
            preds_lower <-
              apply(sims, 1, function(x)
                quantile(x, probs = (1 - self$level / 100) / 2))
            preds_upper <-
              apply(sims, 1, function(x)
                quantile(x, probs = 1 - (1 - self$level / 100) / 2))
            return(list(
              preds = apply(sims, 1, median),
              sims = sims,
              lower = preds_lower,
              upper = preds_upper
            ))
          }
        }
        
        if (method %in% c("splitconformal", "kdesplitconformal"))
        {
          idx_train_calibration <- split_data(self$y_train,
                                              p = 0.5,
                                              seed = self$seed)
          X_train_sc <-
            self$X_train[idx_train_calibration,]
          y_train_sc <-
            self$y_train[idx_train_calibration]
          X_calibration_sc <-
            self$X_train[-idx_train_calibration,]
          y_calibration_sc <-
            self$y_train[-idx_train_calibration]
          fit_obj_train_sc <-
            self$engine$fit(X_train_sc, y_train_sc)
          ################################################################ twice but hard to circumvent
          self$set_model(fit_obj_train_sc)
          ################################################################
          y_pred_calibration <-
            self$engine$predict(self$model, X_calibration_sc)
          abs_residuals <-
            abs(y_calibration_sc - y_pred_calibration)
          preds <-
            self$engine$predict(self$model, X, ...)
          
          if (identical(method, "splitconformal"))
          {
            quantile_absolute_residuals <- quantile_scp(abs_residuals,
                                                        alpha = (1 - self$level / 100))
            return(
              list(
                preds = preds,
                lower = preds - quantile_absolute_residuals,
                upper = preds + quantile_absolute_residuals
              )
            )
          }
          
          if (identical(method, "kdesplitconformal"))
          {
            stopifnot(!is.null(B) && B > 1)
            matrix_preds <-
              replicate(B, preds)
            calibrated_raw_residuals <-
              y_calibration_sc - y_pred_calibration
            scaled_calibrated_residuals <-
              base::scale(calibrated_raw_residuals,
                          center = TRUE,
                          scale = TRUE)
            sd_calibrated_residuals <-
              sd(calibrated_raw_residuals)
            simulated_scaled_calibrated_residuals <-
              rgaussiandens(
                scaled_calibrated_residuals,
                n =
                  length(preds),
                p =
                  B,
                seed =
                  self$seed
              )
            sims <-
              matrix_preds + sd_calibrated_residuals * simulated_scaled_calibrated_residuals
            preds_lower <-
              apply(sims, 1, function(x)
                quantile(x, probs = (1 - self$level / 100) / 2))
            preds_upper <-
              apply(sims, 1, function(x)
                quantile(x, probs = 1 - (1 - self$level / 100) / 2))
            return(list(
              preds = apply(sims, 1, median),
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
                           method = c("splitconformal",
                                      "jackknifeplus",
                                      "kdesplitconformal",
                                      "kdejackknifeplus"),
                           B = 100,
                           seed = 123,
                           graph = FALSE) {
      
      stopifnot(pct_train >= 0.4 && pct_train < 1)
      stopifnot(length(y) == nrow(X))
      method <- match.arg(method)
      
      set.seed(seed)
      train_index <-
        caret::createDataPartition(y, p = pct_train)$Resample1
      X_train <-
        as.matrix(X[train_index,])
      y_train <- y[train_index]
      X_test <-
        as.matrix(X[-train_index,])
      y_test <- y[-train_index]
      
      fit_obj <-
        self$fit(X_train, y_train)
      if (!is.null(self$level))
        # prediction intervals requested
      {
        res <- fit_obj$predict(X_test,
                               method = method,
                               B = B)
        
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
            main = method,
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
        results$method <- method
        results$B <- B
        results$coverage <-
          mean(y_test >= res$lower & y_test <= res$upper)*100
        results$length <-
          mean(res$upper - res$lower)
        
        return(results)
      } else {
        return(score(fit_obj$predict(X_test), y_test))
      }
      
    },
    summary = function(X, level = 95, 
                       show_progress = TRUE, cl = NULL) {
      
      if (is.null(self$engine))
        stop(paste0(self$name, " must be fitted first (use ", self$name, "$fit())"))
      
      if (is_package_available("skimr") == FALSE)
      {
        utils::install.packages("skimr")
      }
      
      deriv_column <- function(ix)
      {
        zero <- 1e-4 
        eps_factor <- zero^(1/3)
        X_plus <- X
        X_minus <- X
        X_ix <- X[, ix]
        cond <- abs(X_ix) > zero
        h <- eps_factor * X_ix * cond + zero * (!cond)
        X_plus[, ix] <- X_ix + h
        X_minus[, ix] <- X_ix - h
        derived_column <- (self$predict(as.matrix(X_plus)) - self$predict(as.matrix(X_minus)))/(2*h)
        return (derived_column)
      }
      deriv_column <- compiler::cmpfun(deriv_column)
      
      deriv_matrix <- parfor(what = deriv_column, 
                             args = seq_len(ncol(X)), 
                             show_progress = show_progress, 
                             verbose = FALSE,
                             combine = cbind,
                             cl = cl)
      
      names_X <- colnames(X)
      if (!is.null(names_X)) 
      {
        colnames(deriv_matrix) <- names_X
      } else {
        colnames(deriv_matrix) <- paste0("V", seq_len(ncol(X)))
      }
      
      foo_tests <- function(x)
      {
        res <- stats::t.test(x)
        return(c(as.numeric(res$conf.int), res$p.value))
      }
      
      lower_signif_codes <- c(0, 0.001, 0.01, 0.05, 0.1)
      upper_signif_codes <- c(0.001, 0.01, 0.05, 0.1, 1)
      signif_codes <- c("***", "**", "*", ".", "")
      choice_signif_code <- function(x) signif_codes[which.max((lower_signif_codes <= x)*(upper_signif_codes > x))]
      ttests <- try(data.frame(t(apply(deriv_matrix, 2, foo_tests))), silent = TRUE)
      if (!inherits(ttests, "try-error"))
      {
        colnames(ttests) <- c("lower", "upper", "p-value")
        ttests$signif <- sapply(ttests[,3], choice_signif_code) # p-values signif.  
        return(list(ttests = ttests,
                    effects = my_skim(deriv_matrix)))
      } else {
        return(my_skim(deriv_matrix))
      }
      
      
    }
  )
)


# 2 - BaseClassifier -----------------------------------------------------------

BaseClassifier <- R6::R6Class(
  classname = "BaseClassifier",
  private = list(encoded_factors = NULL,
                 class_names = NULL),
  public = list(
    name = "BaseClassifier",
    type = "classification",
    model = NULL,
    X_train = NULL,
    y_train = NULL,
    engine = NULL,
    params = NULL,
    initialize = function(name = "BaseClassifier",
                          type = "classification",
                          model = NULL,
                          X_train = NULL,
                          y_train = NULL,
                          engine = NULL,
                          params = NULL) {
      self$name <- name
      self$type <- type
      self$model <- model
      self$X_train <- X_train
      self$y_train <- y_train
      self$engine <- engine
      self$params <- params
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
    set_engine = function(engine) {
      self$engine <- engine
    },
    get_engine = function() {
      self$engine
    },
    get_params = function() {
      self$params
    },
    fit = function(X, y, ...) {
      stopifnot(is.factor(y))
      private$encoded_factors <-
        encode_factors(y)
      private$class_names <-
        as.character(levels(unique(y)))
      Y <- one_hot(y)
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
    predict_proba = function(X, ...) {
      if (is.null(self$engine))
        stop(paste0(self$name, " must be fitted first"))
      raw_preds <-
        expit(self$engine$predict(self$model, X))
      probs <-
        raw_preds / rowSums(raw_preds)
      colnames(probs) <-
        private$class_names
      return(probs)
    },
    predict = function(X, ...) {
      probs <- self$predict_proba(X, ...)
      numeric_factor <-
        apply(probs, 1, which.max)
      res <-
        decode_factors(numeric_factor, private$encoded_factors)
      names(res) <- NULL
      return(res)
    }
  )
)
