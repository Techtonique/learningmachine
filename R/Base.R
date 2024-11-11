

# 1 - Class Base --------------------------------------------------------------

#' `Base` class
#'
#' @description
#' the `Base` class used by other objects; useful 
#' for extensions of the package, not for basic 
#' interactions with the package
#'

Base <-
  R6::R6Class(
    classname = "Base",
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
      #' @param name name of the class
      name = "Base",
      #' @param type type of supervised learning method implemented  
      type = "none",
      #' @param model fitted model 
      model = NULL,
      #' @param method supevised learning method 
      method = NULL,
      #' @param X_train training set features 
      X_train = NULL,
      #' @param y_train training set response
      y_train = NULL,
      #' @param pi_method type of prediction interval in c("splitconformal",
      #' "kdesplitconformal", "bootsplitconformal", "jackknifeplus",
      #' "kdejackknifeplus", "bootjackknifeplus", "surrsplitconformal",
      #' "surrjackknifeplus")
      pi_method = c(
           "none",
           "splitconformal",
           "kdesplitconformal",
           "bootsplitconformal",
           "jackknifeplus",
           "kdejackknifeplus",
           "bootjackknifeplus",
           "surrsplitconformal",
           "surrjackknifeplus"
         ),
      #' @param level an integer; the level of confidence 
      level = 95,
      #' @param B an integer; the number of simulations when \code{level} is not \code{NULL}
      B = 100,
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
      #' @param engine contains fit and predict lower-level methods for the given \code{method}; 
      #' do not modify by hand
      engine = NULL,
      #' @param params additional parameters passed to \code{method} when calling \code{fit}
      params = NULL,
      #' @param seed an integer; reproducibility seed for methods that include 
      #' randomization
      seed = 123,
      #' @description
      #' Create a new object.
      #' @return A new `Base` object.
      initialize = function(name = "Base",
                            type = "none",
                            model = NULL,
                            method = NULL,
                            X_train = NULL,
                            y_train = NULL,
                            pi_method = c(
                              "none",
                              "splitconformal",
                              "kdesplitconformal",
                              "bootsplitconformal",
                              "jackknifeplus",
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
        
        self$name <- name
        self$type <- type
        self$model <- model
        self$method <- method
        self$X_train <- X_train
        self$y_train <- y_train
        self$pi_method <- pi_method
        self$level <- level
        self$B <- B
        self$nb_hidden <- nb_hidden
        self$nodes_sim <- nodes_sim
        self$activ <- activ
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
      get_method = function() {
        self$method
      },
      set_method = function(method) {
        self$method <- method
      },
      get_pi_method = function() {
        self$pi_method
      },
      set_pi_method = function(pi_method) {
        self$pi_method <- pi_method
      },
      get_level = function() {
        self$level
      },
      set_level = function(level) {
        self$level <- level
      },
      get_B = function() {
        self$B
      },
      set_B = function(B) {
        self$B <- B
      },
      get_nb_hidden = function() {
        self$nb_hidden
      },
      set_nb_hidden = function(nb_hidden) {
        self$nb_hidden <- nb_hidden
      },
      get_nodes_sim = function() {
        self$nodes_sim
      },
      set_nodes_sim = function(nodes_sim) {
        self$nodes_sim <- nodes_sim
      },
      get_activ = function() {
        self$activ
      },
      set_activ = function(activ) {
        self$activ <- activ
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
      summary = function(X,
                         show_progress = TRUE,
                         class_name = NULL,
                         class_index = NULL, 
                         y = NULL,
                         type_ci = c("student", "nonparametric", "bootstrap", "conformal"),
                         cl = NULL) {
        if (is.null(self$engine) || is.null(self$model) || is.null(self$type))
          stop(paste0(self$name, " must be fitted first (use ", self$name, "$fit())"))
        
        type_ci <- match.arg(type_ci)
        
        if (self$type == "regression")
        {
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
              100 * (self$predict(as.matrix(X_plus))$preds - self$predict(as.matrix(X_minus))$preds) /
              (2 * h)
            return (derived_column)
          }
          
        } else {
          # classification
          if (is.null(class_name))
          {
            stopifnot(!is.null(class_index))
          } else {
            class_index <-
              get_key_by_value(private$encoded_factors$encoded_factors,
                               class_name) 
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
            probs_plus <- try(self$predict_proba(as.matrix(X_plus))$preds[, class_index], silent=TRUE)
            if (identical(class(probs_plus)[1], "try-error"))
            {
              probs_plus <- self$predict_proba(as.matrix(X_plus))[, class_index]
            }
            probs_minus <- try(self$predict_proba(as.matrix(X_minus))$preds[, class_index], silent=TRUE)
            if (identical(class(probs_minus)[1], "try-error"))
            {
              probs_minus <- self$predict_proba(as.matrix(X_minus))[, class_index]
            }
            derived_column <-
              100 * (probs_plus - probs_minus) / (2 * h)
            return (derived_column)
          }
        }
        
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

        # Student-T tests
        foo_t_tests <- function(x)
        {
          res <- stats::t.test(x)
          return(c(
            as.numeric(res$estimate),
            as.numeric(res$conf.int),
            res$p.value
          ))
        }

        # nonparametric tests 
        foo_nonparam_tests <- function(x)
        {
          res <- try(compute_ci_mean(x), 
                     silent = TRUE)
          if (!inherits(res, "try-error"))
          {
            return(c(
            as.numeric(res$estimate),
            res$lower,
            res$upper,
            res$pvalue
          ))
          } else {
            return(c(0, NA, NA, NA))
          }                    
        }

        # nonparametric tests 
        foo_bootstrap_tests <- function(x)
        {
          res <- try(bootstrap_ci_mean(x), 
                     silent = TRUE)
          if (!inherits(res, "try-error"))
          {
            return(c(
            as.numeric(res$estimate),
            res$lower,
            res$upper,
            res$pvalue
          ))
          } else {
            return(c(0, NA, NA, NA))
          }                    
        }

        foo_conformal_tests <- function(x)
        {
          res <- try(conformal_ci_mean(x), 
                     silent = TRUE)
          if (!inherits(res, "try-error"))
          {
            return(c(
            as.numeric(res$estimate),
            res$lower,
            res$upper,
            res$pvalue
          ))
          } else {
            return(c(0, NA, NA, NA))
          }                    
        }
        
        lower_signif_codes <- c(0, 0.001, 0.01, 0.05, 0.1)
        upper_signif_codes <- c(0.001, 0.01, 0.05, 0.1, 1)
        signif_codes <- c("***", "**", "*", ".", "")
        choice_signif_code <-
          function(x)
            signif_codes[which.max((lower_signif_codes <= x) * (upper_signif_codes > x))]

        if (identical(type_ci, "student"))
          citests <- try(data.frame(t(apply(effects, 2, foo_t_tests))), silent = TRUE)
        
        if (identical(type_ci, "nonparametric"))
          citests <- try(data.frame(t(apply(effects, 2, foo_nonparam_tests))), silent = TRUE)
        
        if (identical(type_ci, "bootstrap"))
          citests <- try(data.frame(t(apply(effects, 2, foo_bootstrap_tests))), silent = TRUE)
        
        if (identical(type_ci, "conformal"))
          citests <- try(data.frame(t(apply(effects, 2, foo_conformal_tests))), silent = TRUE)
        
        #misc::debug_print(citests)

        if (!inherits(citests, "try-error"))
        {
          colnames(citests) <- c("estimate", "lower", "upper", "p-value")                                     
          citests$signif <- sapply(citests[, 4], choice_signif_code) # p-values signif. column       
          
          if (!is.null(y))
          {
            preds <- self$predict(as.matrix(X))
            if (self$pi_method != "none")
            {
              if (self$type == "regression")
              {
                coverage_rate <- 100 * mean((y >= as.numeric(preds$lower)) * (y <= as.numeric(preds$upper)))

                R_squared <- 1 - sum((y - preds$preds) ^ 2) / sum((y - mean(y)) ^ 2)
                R_squared_adj <-
                  1 - (1 - R_squared) * (length(y) - 1) / (length(y) - ncol(X) - 1)

                Residuals <- y - preds$preds
                return(list(
                  R_squared = R_squared,
                  R_squared_adj = R_squared_adj,
                  Residuals = summary(Residuals),
                  Coverage_rate = coverage_rate,
                  citests = citests,
                  signif_codes = "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
                  effects = my_skim(effects)
                ))
              } else { # classification
                probs <- self$predict_proba(as.matrix(X))
                return(list(
                  Coverage_rate = coverage_rate_classifier(y, preds),
                  citests = citests,
                  signif_codes = "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
                  effects = my_skim(effects)
                ))
              }
            } else { # self$pi_method == "none"
              if (self$type == "regression")
              {
                R_squared <- 1 - sum((y - preds) ^ 2) / sum((y - mean(y)) ^ 2)
                R_squared_adj <-
                  1 - (1 - R_squared) * (length(y) - 1) / (length(y) - ncol(X) - 1)
                Residuals <- y - preds
                return(list(
                  R_squared = R_squared,
                  R_squared_adj = R_squared_adj,
                  Residuals = summary(Residuals),
                  citests = citests,
                  signif_codes = "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
                  effects = my_skim(effects)
                ))
              } else { # classification
                return(list(
                  accuracy = mean(y == preds) * 100,
                  citests = citests,
                  signif_codes = "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
                  effects = my_skim(effects)
                ))
              }
            }
          } else {
            return(list(citests = citests,
                        signif_codes = "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
                        effects = my_skim(effects)))
          }
        } else {
          return(my_skim(effects))
        }
      }
    )
  )
