

# 1 - Class Classifier --------------------------------------------------------------

#' `Classifier` class
#'
#' @description
#' 
#' The `Classifier` class contains supervised classification models 
#' 
#' @details
#' 
#' This class implements models: 
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
#' \item{rvfl}{Random Vector Functional Network, see https://www.researchgate.net/publication/332292006_Online_Bayesian_Quasi-Random_functional_link_networks_application_to_the_optimization_of_black_box_functions}
#' }
#' 
Classifier <-
  R6::R6Class(
    classname = "Classifier",
    inherit = learningmachine::Base,
    private = list(
      y = NULL,
      encoded_factors = NULL,
      class_names = NULL,
      n_classes = NULL,
      calib_resids = NULL,
      abs_calib_resids = NULL,
      q_threshold = NULL
    ),
    public = list(
      #' @field name name of the class
      name = "Classifier",
      #' @field type type of supervised learning method implemented  
      type = "classification",
      #' @field model fitted model 
      model = NULL,
      #' @field method supervised learning method in c('lm', 'ranger', 
      #' 'extratrees', 'ridge', 'bcn', 'glmnet', 'krr', 'xgboost') 
      method = NULL,
      #' @field X_train training set features; do not modify by hand 
      X_train = NULL,
      #' @field y_train training set response; do not modify by hand
      y_train = NULL,
      #' @field pi_method type of prediction set in c("splitconformal",
      #' "kdesplitconformal", "bootsplitconformal", 
      #' "surrsplitconformal")
      pi_method = c(
        "none",
        "kdesplitconformal",
        "bootsplitconformal",
        "surrsplitconformal"
      ),
      #' @field level an integer; the level of confidence (default is 95, for 95 per cent)
      #' for prediction sets 
      level = 95,
      #' @field type_prediction_set a string; the type of prediction set (currently, only "score" method)
      type_prediction_set = "score",
      #' @field B an integer; the number of simulations when \code{level} is not \code{NULL}
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
      #' @field engine contains fit and predic lower-level methods for the given \code{method}; 
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
      #' @return A new `Classifier` object.
      initialize = function(name = "Classifier",
                            type = "classification",
                            model = NULL,
                            method = NULL,
                            X_train = NULL,
                            y_train = NULL,
                            pi_method = c(
                              "none",
                              "kdesplitconformal",
                              "bootsplitconformal",
                              "surrsplitconformal"
                            ),
                            level = 95,
                            type_prediction_set = c("none", "score"),
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
        self$type_prediction_set <- match.arg(type_prediction_set)
      },
      get_type_prediction_set = function() {
        self$type_prediction_set
      },
      set_type_prediction_set = function(type_prediction_set) {
        self$type_prediction_set <- type_prediction_set
      },
      #' @description Fit model to training set 
      #' @param X a matrix of covariates (i.e explanatory variables)
      #' @param y a vector, the response (i.e variable to be explained)
      #' @param ... additional parameters to learning algorithm (see vignettes)
      #'
      fit = function(X,
                     y,
                     ...) {
        self$X_train <- X
        self$y_train <- y
        private$y <- y
        private$encoded_factors <- encode_factors(private$y)
        private$class_names <-
          as.character(levels(unique(private$y)))
        private$n_classes <- length(private$class_names)
        self$params <- list(...)
        self$set_engine(
          list(
            fit = function(x, y, ...)
              fit_multitaskregressor(x, y,
                                     method = self$method,
                                     nb_hidden = self$nb_hidden,
                                     nodes_sim = self$nodes_sim,
                                     activ = self$activ,
                                     ...),
            predict = function(objs, X)
              predict_multitaskregressor(objs, X,
                                         method = self$method)
          )
        )
        
        self$set_model(fit_multitaskregressor(
          x = self$X_train,
          y = private$y,
          method = self$method,
          ...
        ))
        
        if (identical(self$pi_method, "none"))
        {
          return (invisible(self))
        }
        
        if (self$pi_method %in% c("kdesplitconformal",
                                  "bootsplitconformal",
                                  "surrsplitconformal"))
        {
          idx_train_calibration <- split_data(
            private$y,
            p = 0.5,
            seed = self$seed,
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
          self$set_model(fit_obj_train_sc)
          
          y_pred_calibration <-
            self$engine$predict(self$model,  # notice the diff
                                X_calibration_sc)
          private$calib_resids <-
            one_hot(y_calibration_sc) - y_pred_calibration
          private$abs_calib_resids <- abs(private$calib_resids)
          
          raw_preds <- expit(self$engine$predict(self$model, 
                                                 X_calibration_sc))
          probs <- sweep(x = raw_preds, 
                         MARGIN = 1, 
                         STATS=rowSums(raw_preds),
                         FUN = "/")
          private$q_threshold <- get_threshold(
            probs = probs,
            y_calibration_sc = one_hot(y_calibration_sc), # must be private$y 
            level = self$level
          )
        }
        
        return(invisible(self))
      },
      predict_proba = function(X) {
        
        if (is.null(self$model) || is.null(self$engine))
          stop(paste0(self$name, " must be fitted first"))
        
        raw_preds <- expit(self$engine$predict(self$model, X))
        
        if (identical(self$pi_method, "none"))
        {
          # no prediction interval on probs
          probs <- sweep(x = raw_preds, 
                         MARGIN = 1, 
                         STATS=rowSums(raw_preds),
                         FUN = "/")
          colnames(probs) <- private$class_names
          
          return(probs)
          
        } else { 
          
          scaled_raw_residuals <- scale(private$calib_resids,
                                        center = TRUE,
                                        scale = TRUE)
          sd_raw_residuals <- apply(private$calib_resids, 2, sd)
          
          set.seed(self$seed)
          
          if (identical(self$pi_method, "kdesplitconformal"))
          {
            simulated_raw_calibrated_residuals <-
              lapply(seq_len(private$n_classes),
                     function(i)
                       rgaussiandens(
                         x = private$calib_resids[, i],
                         n = nrow(raw_preds),
                         p = self$B,
                         seed = self$seed
                       ))
          }
          
          if (identical(self$pi_method, "bootsplitconformal"))
          {
            simulated_raw_calibrated_residuals <-
              lapply(seq_len(private$n_classes),
                     function(i)
                       rbootstrap(
                         x = private$calib_resids[, i],
                         n = nrow(raw_preds),
                         p = self$B,
                         seed = self$seed
                       ))
          }
          
          if (identical(self$pi_method, "surrsplitconformal"))
          {
            if (nrow(raw_preds) > length(private$calib_resids))
            {
              stop(
                "For surrogates, must have number of predictions < number of training observations"
              )
            }
            simulated_raw_calibrated_residuals <-
              lapply(seq_len(private$n_classes),
                     function(i)
                       rsurrogate(
                         x = private$calib_resids[, i],
                         n = nrow(raw_preds),
                         p = self$B,
                         seed = self$seed
                       ))
          }
          
          sims <- lapply(seq_len(private$n_classes), #Rcpp after fix 
                         function (i)
                           replicate(self$B,
                                     raw_preds[, i]) + sd_raw_residuals[i] * simulated_raw_calibrated_residuals[[i]])
          
          q_lower <- 0.5*(1 - self$level / 100)
          q_upper <- 1 - q_lower
          seq_len_n_classes <- seq_len(private$n_classes)
          preds_lower <- lapply(seq_len_n_classes, function(i)
              row_quantiles_cpp(sims[[i]], q = q_lower))
          preds_upper <- lapply(seq_len_n_classes, function(i)
              row_quantiles_cpp(sims[[i]], q = q_upper))
          
          if (!is.null(private$class_names))
          {
            names(sims) <- private$class_names
            names(preds_lower) <- private$class_names
            names(preds_upper) <- private$class_names
          }
          
          res <- list()
          res$preds <- expit_probs(sapply(sims, function(x) rowMeans(x)))
          res$lower <- preds_lower
          res$upper <- preds_upper
          res$sims <- sims # upon request
          return(res)
        }
      },
      predict = function(X) {
        
        probs <- self$predict_proba(X)
        
        if (identical(self$pi_method, "none"))
        {
          
          numeric_factor <- apply(probs, 1, which.max)
          preds <- decode_factors(numeric_factor,
                                  private$encoded_factors)
          names(preds) <- NULL
          return(preds)
          
        } else { # prediction sets required
          
          if (self$type_prediction_set == "score")
          {
            ix_list <- get_classes_idx(probs,
                                       private$q_threshold,
                                       self$level)
            list_classes <- decode_factors2(ix_list)
            
            return(impute_classes(list_classes, probs$preds))
          } else {
            stop("For now, you must set type_prediction_set = 'score' when pi_method == 'none'")
          }
        }
      }
    )
  )


# 2 - utils -------------------------------------------------------------------

fit_multitaskregressor <- function(x,
                                   y,
                                   method = c("lm",
                                              "bcn",
                                              "extratrees",
                                              "glmnet",
                                              "krr",
                                              "ranger",
                                              "ridge",
                                              "xgboost", 
                                              "rvfl"),
                                   show_progress = FALSE,
                                   nb_hidden = 0,
                                   nodes_sim = c("sobol", "halton", "unif"),
                                   activ = c("relu", "sigmoid", "tanh",
                                             "leakyrelu", "elu", "linear"),
                                   ...) {
  if (!is.factor(y))
    y <- as.factor(y)
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
                           reg_lambda = 0.01, ...),
    bcn = function(x, y, ...)
      bcn::bcn(x, y, ...),
    glmnet = function(x, y, ...)
      glmnet::glmnet(x, y, ...),
    krr = function(x, y, ...)
      fit_matern32_regression(x, y,
                              ...),
    xgboost = function(x, y, ...)
      fit_xgboost_regression(x, y, ...), 
    rvfl = function(x, y, ...)
      fit_rvfl_regression(x, y,
                          reg_lambda = 0.01, ...)
  )
  res <- vector("list", length = n_classes)
  names(res) <- class_names
  
  if(nb_hidden == 0)
  {
    for (i in 1:n_classes) {
      res[[i]] <- obj(x = x, y = Y[, i], ...)
    }
  } else { # nb_hidden > 0
    nodes_sim <- match.arg(nodes_sim)
    activ <- match.arg(activ)
    new_predictors <- create_new_predictors(x, 
                                            nb_hidden = nb_hidden,
                                            nodes_sim = nodes_sim,
                                            activ = activ)
    for (i in 1:n_classes) {
      res[[i]] <- obj(x = new_predictors$predictors, 
                      y = Y[, i], ...)
      res[[i]]$new_predictors <- new_predictors
      res[[i]]$nb_hidden <- nb_hidden
      res[[i]]$nodes_sim <- nodes_sim
      res[[i]]$activ <- activ
    }
  }
  return(res)
}


predict_multitaskregressor <- function(objs,
                                       X,
                                       method = c("lm",
                                                  "bcn",
                                                  "extratrees",
                                                  "glmnet",
                                                  "krr",
                                                  "ranger",
                                                  "ridge",
                                                  "xgboost",
                                                  "rvfl")) {
  method <- match.arg(method)
  predict_func <- switch(
    method,
    lm = function(obj, X)
      X %*% obj$coefficients,
    bcn = bcn::predict.bcn,
    extratrees = predict_func_extratrees,
    glmnet = glmnet::predict.glmnet,
    krr = predict_matern32,
    ranger = predict_func_ranger,
    ridge = predict_ridge_regression,
    xgboost = predict,
    rvfl = predict_rvfl_regression
  )
  
  preds <- matrix(NA, nrow = nrow(X),
                  ncol = length(objs))
  
  if (!identical(method, "rvfl"))
  {
    if (is.null(objs[[1]]$nb_hidden))
    {
      
      for (i in 1:length(objs)) {
        preds[, i] <- predict_func(objs[[i]], X)
      }
      
    } else {
      
      nb_hidden <- objs[[1]]$nb_hidden
      if (is.null(nb_hidden))
        nb_hidden <- 0  
      newX <- create_new_predictors(X, 
                                    nb_hidden = nb_hidden,
                                    nodes_sim = objs[[1]]$nodes_sim,
                                    activ = objs[[1]]$activ,
                                    nn_xm = objs[[1]]$new_predictors$nn_xm, 
                                    nn_scales = objs[[1]]$new_predictors$nn_scales)
      for (i in 1:length(objs)) {
        preds[, i] <- predict_func(objs[[i]], newX$predictors)
      }
    }
    
  } else {
    
    for (i in 1:length(objs)) {
      preds[, i] <- predict_func(objs[[i]], X)
    }
    
  }
  
  return(preds)
}