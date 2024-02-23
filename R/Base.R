
# 1 - Class Base --------------------------------------------------------------

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
      name = "Base",
      type = "none",
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
      initialize = function(name = "Base",
                            type = "none",
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
        self$name <- name
        self$type <- type
        self$model <- model
        self$method <- method
        self$X_train <- X_train
        self$y_train <- y_train
        self$pi_method <- pi_method
        self$level <- level
        self$B <- B
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
                         level = 95,
                         show_progress = TRUE,
                         class_name = NULL,
                         y = NULL,
                         cl = NULL) {
        
        if (is.null(self$engine) || is.null(self$model) || is.null(self$type))
          stop(paste0(self$name, " must be fitted first (use ", self$name, "$fit())"))
        
        if (is_package_available("skimr") == FALSE)
        {
          utils::install.packages("skimr")
        }
        
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
              100*(self$predict(as.matrix(X_plus))$preds - self$predict(as.matrix(X_minus))$preds) /
              (2 * h)
            return (derived_column)
          }
        } else { # classification
          stopifnot(!is.null(class_name))
          class_index <- get_key_by_value(private$encoded_factors$encoded_factors, 
                                          class_name)
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
            probs_plus <- self$predict_proba(as.matrix(X_plus))[, class_index]
            probs_minus <- self$predict_proba(as.matrix(X_minus))[, class_index]
            derived_column <- 100*(probs_plus - probs_minus) / (2 * h)
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
            if (!is.null(self$level))
            {
              coverage_rate <- coverage_rate_classifier(y_test = y, 
                                                        preds_set_list = preds$preds)
            } else {
              coverage_rate <- NULL
            }
            return(
              list(
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
