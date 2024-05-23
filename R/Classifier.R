

# 1 - Class Classifier --------------------------------------------------------------

#' `Classifier` class
#'
#' @description
#' the `Classifier` class contains supervised classification models 
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
      type_split = NULL,
      calib_resids = NULL,
      abs_calib_resids = NULL,
      q_threshold = NULL
    ),
    public = list(
      #' @param name name of the class
      name = "Classifier",
      #' @param type type of supervised learning method implemented  
      type = "classification",
      #' @param model fitted model 
      model = NULL,
      #' @param method supervised learning method in c('lm', 'ranger', 
      #' 'extratrees', 'ridge', 'bcn', 'glmnet', 'krr', 'xgboost') 
      method = NULL,
      #' @param X_train training set features; do not modify by hand 
      X_train = NULL,
      #' @param y_train training set response; do not modify by hand
      y_train = NULL,
      #' @param pi_method type of prediction set in c("splitconformal",
      #' "kdesplitconformal", "bootsplitconformal", "jackknifeplus",
      #' "kdejackknifeplus", "bootjackknifeplus", "surrsplitconformal",
      #' "surrjackknifeplus")
      pi_method = NULL,
      #' @param level an integer; the level of confidence (default is 95, for 95%)
      #' for prediction sets 
      level = NULL,
      #' @param type_prediction_set a string; the type of prediction set (currently, only "score" method)
      type_prediction_set = "score",
      #' @param B an integer; the number of simulations when \code{level} is not \code{NULL}
      B = NULL,
      #' @param nb_hidden number of nodes in the hidden layer, for construction of a quasi-
      #' randomized network 
      nb_hidden = 0,
      #' @param nodes_sim type of 'simulations' for hidden nodes, if \code{nb_hidden} > 0; 
      #' takes values in c("sobol", "halton", "unif") 
      nodes_sim = c("sobol", "halton", "unif"),
      #' @param activ activation function's name for the hidden layer, in the construction 
      #' of a quasi-randomized network; takes values in c("relu", "sigmoid", "tanh", "
      #' leakyrelu", "elu", "linear")
      activ = c("relu", "sigmoid", "tanh",
                "leakyrelu", "elu", "linear"),
      #' @param engine contains fit and predic lower-level methods for the given \code{method}; 
      #' do not modify by hand
      engine = NULL,
      #' @param params additional parameters passed to \code{method} when calling \code{fit}
      #' do not modify by hand 
      params = NULL,
      #' @param seed an integer; reproducibility seed for methods that include 
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
                            pi_method = NULL,
                            level = NULL,
                            type_prediction_set = "score",
                            B = NULL,
                            nb_hidden = 0,
                            nodes_sim = c("sobol", "halton", "unif"),
                            activ = c("relu", "sigmoid", "tanh",
                                      "leakyrelu", "elu", "linear"),
                            engine = NULL,
                            params = NULL,
                            seed = 123) {
        
        nodes_sim <- match.arg(nodes_sim)
        activ <- match.arg(activ)
        
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
                     pi_method = c("none",
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
        private$class_names <-
          as.character(levels(unique(private$y)))
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
                                     nb_hidden = self$nb_hidden,
                                     nodes_sim = self$nodes_sim,
                                     activ = self$activ,
                                     ...),
            predict = function(objs, X)
              predict_multitaskregressor(objs, X,
                                         method = self$method)
          )
        )
        
        if (identical(self$pi_method, "none") && is.null(self$level))
        {
          self$set_model(fit_multitaskregressor(
            x = self$X_train,
            y = private$y,
            method = self$method,
            ...
          ))
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
        
        if (self$pi_method %in% c("kdesplitconformal",
                                  "bootsplitconformal",
                                  "surrsplitconformal"))
        {
          idx_train_calibration <- split_data(
            private$y,
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
          self$set_model(fit_obj_train_sc)
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
            y_pred_calibration <-
              self$engine$predict(self$model,  # notice the diff
                                  X_calibration_sc)
            private$calib_resids <-
              one_hot(y_calibration_sc) - y_pred_calibration
            private$abs_calib_resids <- abs(private$calib_resids)
          }
          
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
      predict_proba = function(X, level=NULL) {
        
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
          
          scaled_raw_residuals <- scale(private$calib_resids,
                                        center = TRUE,
                                        scale = TRUE)
          sd_raw_residuals <- apply(private$calib_resids, 2, sd)
          
          set.seed(self$seed)
          
          if (self$pi_method %in% c("kdejackknifeplus", "kdesplitconformal"))
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
          
          if (self$pi_method %in% c("bootjackknifeplus", "bootsplitconformal"))
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
          
          if (self$pi_method %in% c("surrsplitconformal", "surrjackknifeplus"))
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
          debug_print(sims)
          preds_lower <-
            lapply(seq_len(private$n_classes), function(i)
              pmax(0, apply(sims[[i]], 1, function(x)
                quantile(x, probs = (1 - self$level / 100) / 2))))
          debug_print(preds_lower)
          preds_upper <-
            lapply(seq_len(private$n_classes), function(i)
              pmin(1, apply(sims[[i]], 1, function(x)
                quantile(x, probs = 1 - (1 - self$level / 100) / 2))))
          debug_print(preds_upper)
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
      predict = function(X,
                         level = NULL) {
        
        probs <- self$predict_proba(X)
        
        if (is.null(level) && is.null(self$level))
        {
          numeric_factor <- apply(probs, 1, which.max)
          preds <- decode_factors(numeric_factor,
                                  private$encoded_factors)
          names(preds) <- NULL
          return(preds)
          
        } else { # !is.null(level) || !is.null(self$level)
          
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
            
            return(impute_classes(list_classes, probs$preds))
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
                                              "xgboost"),
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
                                                  "xgboost")) {
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
    xgboost = predict
  )
  preds <- matrix(NA, nrow = nrow(X),
                  ncol = length(objs))
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
  return(preds)
}