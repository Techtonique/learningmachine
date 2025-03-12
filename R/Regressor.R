
# 1 - Class Regressor --------------------------------------------------------------

#' `Regressor` class
#'
#' @description The `Regressor` class contains supervised regression models 
#' 
#' @details This class implements models: 
#' 
#' \describe{
#' \item{lm}{Linear model}
#' \item{bcn}{see https://www.researchgate.net/publication/380760578_Boosted_Configuration_neural_Networks_for_supervised_classification}
#' \item{extratrees}{Extremely Randomized Trees; see https://link.springer.com/article/10.1007/s10994-006-6226-1}
#' \item{glmnet}{Elastic Net Regression; see https://glmnet.stanford.edu/}
#' \item{krr}{Kernel Ridge Regression; see for example https://www.jstatsoft.org/article/view/v079i03} (but the implementation is different)
#' \item{ranger}{Random Forest; see https://www.jstatsoft.org/article/view/v077i01}
#' \item{ridge}{Ridge regression; see https://arxiv.org/pdf/1509.09169}
#' \item{xgboost}{a scalable tree boosting system see https://arxiv.org/abs/1603.02754}
#' \item{svm}{Support Vector Machines, see https://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf}
#' \item{rvfl}{Random Vector Functional Network, see https://www.researchgate.net/publication/332292006_Online_Bayesian_Quasi-Random_functional_link_networks_application_to_the_optimization_of_black_box_functions}
#' }
#' 
Regressor <-
  R6::R6Class(
    classname = "Regressor",
    inherit = learningmachine::Base,
    private = list(
      type_split = NULL,
      calib_resids = NULL,
      abs_calib_resids = NULL
    ),
    public = list(
      #' @field name name of the class
      name = "Regressor",
      #' @field type type of supervised learning method implemented  
      type = "regression",
      #' @field model fitted model 
      model = NULL,
      #' @field method supervised learning method in c('lm', 'ranger', 
      #' 'extratrees', 'ridge', 'bcn', 'glmnet', 'krr', 
      #' 'xgboost', 'svm') 
      method = NULL,
      #' @field X_train training set features; do not modify by hand 
      X_train = NULL,
      #' @field y_train training set response; do not modify by hand
      y_train = NULL,
      #' @field pi_method type of prediction interval in c("splitconformal",
      #' "kdesplitconformal", "bootsplitconformal", "jackknifeplus",
      #' "kdejackknifeplus", "bootjackknifeplus", "surrsplitconformal",
      #' "surrjackknifeplus")
      pi_method = c(
        "none",
        "splitconformal",
        "jackknifeplus",
        "kdesplitconformal",
        "bootsplitconformal",
        "kdejackknifeplus",
        "bootjackknifeplus",
        "surrsplitconformal",
        "surrjackknifeplus"
      ),
      #' @field level an integer; the level of confidence (default is 95, for 95 per cent)
      #' for prediction intervals
      level = 95,
      #' @field B an integer; the number of simulations when 'level' is not NULL
      B = 100,
      #' @field nb_hidden number of nodes in the hidden layer, for construction of a quasi-
      #' randomized network 
      nb_hidden = 0,
      #' @field nodes_sim type of 'simulations' for hidden nodes, if \code{nb_hidden} > 0; 
      #' takes values in c("sobol", "halton", "unif") 
      nodes_sim = c("sobol", "halton", "unif"),
      #' @field activ activation function's name for the hidden layer, in the construction 
      #' of a quasi-randomized network; takes values in c("relu", "sigmoid", "tanh", "
      #' leakyrelu", "elu", "linear")
      activ = c("relu", "sigmoid", "tanh",
                "leakyrelu", "elu", "linear"),
      #' @field engine contains fit and predict lower-level methods for the given \code{method}; 
      #' do not modify by hand
      engine = NULL,
      #' @field params additional parameters passed to \code{method} when calling \code{fit}
      #' do not modify by hand 
      params = NULL,
      #' @field seed an integer; reproducibility seed for methods that include 
      #' randomization
      seed = 123,
      #' @description
      #' Create a new object.
      #' @return A new `Regressor` object.
      initialize = function(name = "Regressor",
                            type = "regression",
                            model = NULL,
                            method = NULL,
                            X_train = NULL,
                            y_train = NULL,
                            pi_method = c("none",
                              "splitconformal",
                              "jackknifeplus",
                              "kdesplitconformal",
                              "bootsplitconformal",
                              "kdejackknifeplus",
                              "bootjackknifeplus",
                              "surrsplitconformal",
                              "surrjackknifeplus"
                            ),
                            level = 95,
                            B = 100,
                            nb_hidden = 0,
                            nodes_sim = c("sobol", "halton", "unif"),
                            activ = c("relu", "sigmoid", "tanh",
                                      "leakyrelu", "elu", "linear"),
                            engine = NULL,
                            params = NULL,
                            seed = 123) {
        
        nodes_sim <- match.arg(nodes_sim)
        activ <- match.arg(activ)
        pi_method <- match.arg(pi_method)
        
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
          nb_hidden = nb_hidden,
          nodes_sim = nodes_sim,
          activ = activ,
          engine = engine,
          params = params,
          seed = seed
        )
      },
      #' @description Fit model to training set 
      #' @param X a matrix of covariates (i.e explanatory variables)
      #' @param y a vector, the response (i.e variable to be explained)
      #' @param type_split type of data splitting for split conformal prediction: 
      #' "stratify" (for classical supervised learning) "sequential" (when 
      #' the data sequential ordering matters)
      #' @param ... additional parameters to learning algorithm (see vignettes)
      #'
      fit = function(X,
                     y,
                     type_split = c("stratify",
                                    "sequential"),
                     ...) {
        self$X_train <- X
        self$y_train <- drop(y)
        type_split <- match.arg(type_split)
        private$type_split <- type_split
        self$params <- list(...)
        self$set_engine(list(
          fit = function(x, y, ...)
            fit_regressor(x, y,
                          method = self$method,
                          nb_hidden = self$nb_hidden,
                          nodes_sim = self$nodes_sim,
                          activ = self$activ,
                          ...),
          predict = function(obj, X)
            predict_regressor(obj, X,
                              method = self$method)
        ))

        if (identical(self$method, "bayesianrvfl"))
        {
          self$set_pi_method("bayesian")
        }
        
        if (self$pi_method %in% c("none", "bayesian"))
        {
          self$set_model(fit_regressor(
            x = self$X_train,
            y = self$y_train,
            method = self$method,
            nb_hidden = self$nb_hidden,
            nodes_sim = self$nodes_sim,
            activ = self$activ,
            ... 
          )) 
        }
        
        if (self$pi_method %in% c("splitconformal", 
                                  "kdesplitconformal"))
        {
          idx_train_calibration <- split_data(
            self$y_train,
            p = 0.5,
            seed = self$seed,
            type_split = private$type_split
          )
          X_train_sc <-
            self$X_train[idx_train_calibration,] # training set 
          y_train_sc <-
            self$y_train[idx_train_calibration] # training set
          X_calibration_sc <-
            self$X_train[-idx_train_calibration,] # calibration set
          y_calibration_sc <-
            self$y_train[-idx_train_calibration] # calibration set
          
          fit_obj_train_sc <- self$engine$fit(X_train_sc,
                                              y_train_sc,
                                              ...)
          self$set_model(fit_obj_train_sc) # /!\ keep 
          if (private$type_split == "sequential")
          {
            y_pred_calibration <-
              self$engine$predict(fit_obj_train_sc, # notice the diff
                                  X_calibration_sc)
            private$calib_resids <- y_calibration_sc - y_pred_calibration
            private$abs_calib_resids <- abs(private$calib_resids)
            self$set_model(self$engine$fit(X_calibration_sc,
                                           y_calibration_sc))
          } else { # not in sequential order 
            y_pred_calibration <- self$engine$predict(self$model,  # notice the diff
                                  X_calibration_sc)
            private$calib_resids <- y_calibration_sc - y_pred_calibration
            private$abs_calib_resids <- abs(private$calib_resids)
          }
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
            self$set_model(self$engine$fit(self$X_train,
                                           self$y_train))
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
            self$set_model(self$engine$fit(self$X_train,
                                           self$y_train))
          }
        }
        
        return(invisible(self))
        
      },
      #' @description Predict model on test set 
      #' @param X a matrix of covariates (i.e explanatory variables)
      #' @param ... additional parameters 
      predict = function(X, ...) {
        if (is.null(self$model) || is.null(self$engine))
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
        if (self$pi_method %in% c("none", "bayesian"))
        {          
          return(preds)
        } else {
          if (self$pi_method %in% c("splitconformal", 
                                    "jackknifeplus"))
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
          
          if (self$pi_method %in% c("kdejackknifeplus",
                                    "bootjackknifeplus",
                                    "surrjackknifeplus",
                                    "kdesplitconformal",
                                    "bootsplitconformal",
                                    "surrsplitconformal"))
          {
            scaled_raw_residuals <- scale(private$calib_resids, 
                                          center = TRUE,
                                          scale = TRUE)
            
            sd_raw_residuals <- sd(private$calib_resids)
            
            set.seed(self$seed)
            
            if (self$pi_method %in% c("kdejackknifeplus", "kdesplitconformal"))
            {
              simulated_raw_calibrated_residuals <- rgaussiandens(x = private$calib_resids,
                                                                  n = length(preds),
                                                                  p = self$B,
                                                                  seed = self$seed)
            }
            
            if (self$pi_method %in% c("bootjackknifeplus", "bootsplitconformal"))
            {
              simulated_raw_calibrated_residuals <- rbootstrap(x = private$calib_resids,
                                                               n = length(preds),
                                                               p = self$B,
                                                               seed = self$seed)
            }
            
            if (self$pi_method %in% c("surrsplitconformal", "surrjackknifeplus"))
            {
              if (length(preds) > length(private$calib_resids))
              {
                stop("For surrogates, must have number of predictions < number of training observations")
              }
              simulated_raw_calibrated_residuals <- rsurrogate(x = private$calib_resids,
                                                               n = length(preds),
                                                               p = self$B,
                                                               seed = self$seed)
            }
            
            q_lower <- 0.5*(1 - self$level / 100)
            q_upper <- 1 - q_lower
            sims <-
              replicate(self$B, preds) + sd_raw_residuals * simulated_raw_calibrated_residuals
            preds_lower <-
              apply(sims, 1, function(x)
                empirical_quantile_cpp(x, q = q_lower))
            preds_upper <-
              apply(sims, 1, function(x)
                empirical_quantile_cpp(x, q = q_upper))
            return(list(
              preds = apply(sims, 1, mean),
              sims = sims,
              lower = preds_lower,
              upper = preds_upper
            ))
          }
        }
      },
      #' @description Fit model to training set and predict on test set
      #' 
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
                               "none",
                               "splitconformal",
                               "jackknifeplus",
                               "kdesplitconformal",
                               "bootsplitconformal",
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
        
        self$params <- list(...)
        pi_method <- match.arg(pi_method)
        self$pi_method <- pi_method
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
                            ...)
        
        if (!is.null(self$level))
          # prediction intervals requested
        {
          res <- fit_obj$predict(X_test, level = self$level)
          
          if ((graph == TRUE) &&
              (!is.factor(y))) {
            y_values <- c(y_train, 
                          res$preds)
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
          
          results$level <- self$level
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
      #' @description update model in an online fashion (for now, only implemented for 'rvfl' models")
      #' @param newx a vector of new covariates (i.e explanatory variables)
      #' @param newy a numeric, the new response's observation (i.e variable to be explained)
      #' @param ... additional parameters to be passed to the underlying model's method `update`
      update = function(newx, newy, ...)
      {
        if(!self$method %in% c("rvfl", "bayesianrvfl"))
        {
          stop(paste0("As of ", Sys.Date(), ", this method is only implemented for 'rvfl' models"))
        }
        self$set_model(update_rvfl_regressor(self$model, 
          newx=as.numeric(newx), newy=newy, ...))
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
                                     "krr",
                                     "xgboost", 
                                     "svm", 
                                     "rvfl",
                                     "bayesianrvfl"),
                          nb_hidden = 0,
                          nodes_sim = c("sobol", "halton", "unif"),
                          activ = c("relu", "sigmoid", "tanh",
                                    "leakyrelu", "elu", "linear"),
                          ...) {
  
  regressor_choice <- match.arg(method)
  obj <- switch(
    regressor_choice,
    lm = function(x, y, ...)
      stats::.lm.fit(x, y),
    ranger = function(x, y, ...) fit_func_ranger_regression(x, y, ...),
    extratrees = function(x, y, ...) fit_func_extratrees_regression(x, y, ...),
    ridge = function(x, y, ...)
      fit_ridge_regression(x, y, ...),
    bcn = function(x, y, ...)
      bcn::bcn(x, y, ...),
    glmnet = function(x, y, ...)
      glmnet::glmnet(x, y, ...),
    krr = function(x, y, ...)
      fit_matern32_regression(x, y,
                              ...),
    xgboost = function(x, y, ...)
      fit_xgboost_regression(x, y, ...),
    svm = function(x, y, ...)
      e1071::svm(x = x, y = y, ...),
    rvfl = function(x, y, ...) fit_rvfl_regression(x, y, ...),
    bayesianrvfl = function(x, y, ...) fit_bayesianrvfl_regression(x, y, ...)
  )
  
  if(nb_hidden == 0)
  {
    
    if (!(regressor_choice %in% c("bayesianrvfl", "rvfl")))
    {
      return(obj(x = x, 
                 y = y, 
                 ...)) 
    } else {
      stop("for 'rvfl', must have 'nb_hidden' > 0")
    }
    
  } else { # nb_hidden > 0
    
    nodes_sim <- match.arg(nodes_sim)
    activ <- match.arg(activ)
    
    if (!(regressor_choice %in% c("bayesianrvfl", "rvfl")))
    {
      new_predictors <- create_new_predictors(x, 
                                              nb_hidden = nb_hidden,
                                              nodes_sim = nodes_sim,
                                              activ = activ)
      obj <- obj(x = new_predictors$predictors, 
                 y = y, 
                 ...)
      obj$new_predictors <- new_predictors
      obj$nb_hidden <- nb_hidden
      obj$nodes_sim <- nodes_sim
      obj$activ <- activ
      
    } else { # regressor_choice %in% c("bayesianrvfl", "rvfl")
      
      obj <- obj(x = x, 
                 y = y, 
                 nb_hidden = nb_hidden,
                 nodes_sim = nodes_sim,
                 activ = activ,
                 ...)
    }
    
    return(obj) 
  }
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
                                         "xgboost",
                                         "svm",
                                         "rvfl", 
                                         "bayesianrvfl"),
                              ...) {
  method_choice <- match.arg(method)
  predict_func <- switch(
    method_choice,
    lm = function(object, X) X %*% object$coefficients,
    bcn = bcn::predict.bcn,
    extratrees = predict_func_extratrees,
    glmnet = predict,
    krr = predict_matern32,
    ranger = predict_func_ranger,
    ridge = predict_ridge_regression,
    xgboost = predict,
    svm = predict,
    rvfl = predict_rvfl_regression,
    bayesianrvfl = predict_bayesianrvfl_regression
  )
  if (is.null(obj$new_predictors))
  {
    return(predict_func(obj, X, ...)) # '...' for example for bayesianrvfl::predict_rvfl 
  } else {
    newX <- create_new_predictors(X, 
                                  nb_hidden = obj$nb_hidden,
                                  nodes_sim = obj$nodes_sim,
                                  activ = obj$activ,
                                  nn_xm = obj$new_predictors$nn_xm, 
                                  nn_scales = obj$new_predictors$nn_scales)
    return(predict_func(obj, newX$predictors))
  }
}
