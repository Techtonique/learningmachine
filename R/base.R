

# 1 - BaseRegressor -----------------------------------------------------------

BaseRegressor <- R6::R6Class("BaseRegressor",
                             public = list(
                               name = "BaseRegressor",
                               type = "regression",
                               model = NULL,
                               X_train = NULL,
                               y_train = NULL,
                               engine = NULL, 
                               params = NULL,
                               seed = 123,
                               initialize = function(name = "BaseRegressor",
                                                     type = "regression",
                                                     model = NULL,
                                                     X_train = NULL,
                                                     y_train = NULL,
                                                     engine = NULL, 
                                                     params = NULL,
                                                     seed = 123) {
                                 self$name <- name
                                 self$type <- type
                                 self$model <- model
                                 self$X_train <- X_train
                                 self$y_train <- y_train
                                 self$engine <- engine
                                 self$params <- params
                                 self$seed <- seed                                  
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
                               set_seed = function(seed) {
                                 self$seed <- seed
                               },
                               fit = function(X, y, ...) {
                                 self$X_train <- X
                                 self$y_train <- y
                                 self$params <- list(...)
                                 self$set_model(stats::.lm.fit(x = self$X_train,
                                                               y = self$y_train,
                                                               ...))
                                 self$set_engine(list(fit = stats::.lm.fit, 
                                                      predict = function(obj, X) drop(X%*%obj$coefficients)))
                                 return(base::invisible(self))
                               },
                               predict = function(X, level = NULL,
                                                  method = c("splitconformal",
                                                             "jackknifeplus",
                                                             "kdesplitconformal",
                                                             "kdejackknifeplus"),
                                                  B = 250,  
                                                  ...) {
                                 
                                 if (is.null(self$model) || is.null(self$engine))
                                   stop(paste0(self$name, " must be fitted first"))
                                 
                                 if (is.null(level)) # no prediction interval
                                 {
                                   
                                   return(self$engine$predict(self$model, X))
                                   
                                 } else { # prediction intervals
                                   
                                   stopifnot(is_wholenumber(level))
                                   stopifnot(level > 49 && level < 100)
                                   method <- match.arg(method)
                                   
                                   if (method %in% c("jackknifeplus", "kdejackknifeplus"))
                                   {
                                     stopifnot(!is.null(self$X_train))
                                     stopifnot(!is.null(self$y_train))
                                     n_train <- nrow(self$X_train)
                                     fit_objs_loocv <- base::vector("list", n_train)
                                     abs_residuals_loocv <- rep(0, n_train)
                                     raw_residuals_loocv <- rep(0, n_train)
                                     
                                     pb <- txtProgressBar(min = 1, max = n_train, style = 3)
                                     for (i in 1:n_train) {
                                       left_out_indices <- setdiff(1:n_train, i)
                                       obj_jackknife_residuals <- get_jackknife_residuals(X = self$X_train, 
                                                                                          y = self$y_train, 
                                                                                          idx = left_out_indices, 
                                                                                          fit_func = self$engine$fit,
                                                                                          predict_func = self$engine$predict)
                                       abs_residuals_loocv[i] <- obj_jackknife_residuals$abs_residuals
                                       raw_residuals_loocv[i] <- obj_jackknife_residuals$raw_residuals
                                       setTxtProgressBar(pb, i)
                                     }
                                     close(pb)
                                     
                                     preds <- self$engine$predict(self$model, X, ...) #/!\
                                     
                                     if (identical(method, "jackknifeplus"))
                                     {
                                       quantile_absolute_residuals <- quantile(abs_residuals_loocv, # abs or raw
                                                                               level/100)
                                       return(list(preds = preds,
                                                   lower = preds - quantile_absolute_residuals,
                                                   upper = preds + quantile_absolute_residuals))
                                     }
                                     
                                     if (identical(method, "kdejackknifeplus"))
                                     {
                                       stopifnot(!is.null(B) && B > 1)    
                                       matrix_preds <- replicate(B, preds)
                                       scaled_raw_residuals <- base::scale(raw_residuals_loocv, 
                                                                           center = TRUE, 
                                                                           scale = TRUE)  
                                       sd_raw_residuals <- sd(raw_residuals_loocv)
                                       simulated_raw_calibrated_residuals <- rgaussiandens(scaled_raw_residuals, 
                                                                                           n=length(preds),
                                                                                           p=B,                                                                               
                                                                                           seed=self$seed) 
                                       sims <- matrix_preds + sd_raw_residuals*simulated_raw_calibrated_residuals 
                                       preds_lower <- apply(sims, 1, function(x) quantile(x, probs = (1 - level / 100) / 2))
                                       preds_upper <- apply(sims, 1, function(x) quantile(x, probs = 1 - (1 - level / 100) / 2))                                      
                                       return(list(preds = apply(sims, 1, median),
                                                   sims = sims,
                                                   lower = preds_lower,
                                                   upper = preds_upper))
                                     }
                                   }
                                   
                                   if (method %in% c("splitconformal", "kdesplitconformal"))
                                   {
                                     idx_train_calibration <- split_data(self$y_train,
                                                                         p = 0.5,
                                                                         seed = self$seed)                                    
                                     X_train_sc <- self$X_train[idx_train_calibration, ]
                                     y_train_sc <- self$y_train[idx_train_calibration]
                                     X_calibration_sc <- self$X_train[-idx_train_calibration, ]
                                     y_calibration_sc <- self$y_train[-idx_train_calibration]
                                     fit_obj_train_sc <- self$engine$fit(X_train_sc, y_train_sc)
                                     y_pred_calibration <- self$engine$predict(self$model, X_calibration_sc)
                                     abs_residuals <- abs(y_calibration_sc - y_pred_calibration)                                     
                                     preds <- self$engine$predict(self$model, X, ...)
                                     
                                     if (identical(method, "splitconformal"))
                                     {                                      
                                       quantile_absolute_residuals <- quantile_scp(abs_residuals, 
                                                                                   alpha = (1 - level / 100))
                                       return(list(preds = preds,
                                                   lower = preds - quantile_absolute_residuals,
                                                   upper = preds + quantile_absolute_residuals))
                                     }
                                     
                                     if (identical(method, "kdesplitconformal"))
                                     {
                                       stopifnot(!is.null(B) && B > 1)    
                                       matrix_preds <- replicate(B, preds)
                                       calibrated_raw_residuals <- y_calibration_sc - y_pred_calibration
                                       scaled_calibrated_residuals <- base::scale(calibrated_raw_residuals, 
                                                                                  center = TRUE, 
                                                                                  scale = TRUE)    
                                       sd_calibrated_residuals <- sd(calibrated_raw_residuals)
                                       simulated_scaled_calibrated_residuals <- rgaussiandens(scaled_calibrated_residuals, 
                                                                                              n=length(preds),
                                                                                              p=B,                                                                               
                                                                                              seed=self$seed) 
                                       sims <- matrix_preds + sd_calibrated_residuals*simulated_scaled_calibrated_residuals 
                                       preds_lower <- apply(sims, 1, function(x) quantile(x, probs = (1 - level / 100) / 2))
                                       preds_upper <- apply(sims, 1, function(x) quantile(x, probs = 1 - (1 - level / 100) / 2))                                      
                                       return(list(preds = apply(sims, 1, median),
                                                   sims = sims,
                                                   lower = preds_lower,
                                                   upper = preds_upper))
                                     }                                                                               
                                     
                                   }                                                                                                         
                                 }
                               },
                               fit_predict = function(X, y, 
                                                      pct_train=0.8,                                                                                                             
                                                      score=ifelse(is.factor(y), 
                                                                  yes = function(preds, y_test) mean(preds == y_test), 
                                                                  no = function(preds, y_test) sqrt(mean((preds - y_test)^2))),
                                                      level = NULL,
                                                      method = c("splitconformal",
                                                             "jackknifeplus",
                                                             "kdesplitconformal",
                                                             "kdejackknifeplus"),
                                                      B = 100,
                                                      seed=123, 
                                                      graph=FALSE,                         
                                                      ...) {

                                        stopifnot(pct_train >= 0.4 && pct_train < 1)
                                        stopifnot(length(y) == nrow(X))
                                        method <- match.arg(method)
                                        
                                        set.seed(seed)
                                        train_index <- caret::createDataPartition(y, p=pct_train)$Resample1
                                        X_train <- as.matrix(X[train_index, ])
                                        y_train <- y[train_index]
                                        X_test <- as.matrix(X[-train_index, ])
                                        y_test <- y[-train_index]

                                        fit_obj <- self$fit(X_train, y_train, ...)
                                        if (!is.null(level))
                                        {
                                          res <- fit_obj$predict(X_test, level=level, method=method, B=B, ...)
                                        } else {
                                          res <- fit_obj$predict(X_test, ...)
                                        }

                                        if (graph && !is.factor(y) && !is.null(level)) { 
                                            y_values <- c(y_train, res$preds)
                                            y_upper <- c(y_train, res$upper)
                                            y_lower <- c(y_train, res$lower)

                                            x <- 1:length(y_values)
                                            xx <- c(x, rev(x))
                                            yy <- c(y_lower, rev(y_upper))                                          
                                            plot(x, y_values, type='l',
                                                main=method,
                                                xlab="obs. index",
                                                ylab="values",
                                                ylim = c(min(c(y_lower, y_upper, y_test)),
                                                         max(c(y_lower, y_upper, y_test))))
                                            polygon(xx, yy, col = "gray80", border = "gray80")                                            
                                            lines(y_values, col = "red")
                                            lines(c(y_train, y_test), col = "blue")                                            
                                        }
                                        
                                          # point prediction
                                          try_res <- try(score(res$preds, y_test), silent = FALSE)
                                          if(inherits(try_res, "try-error")) stop("check scoring method") else return(try_res)                                          
                                        
                                          # probabilistic prediction (can use res$lower, res$upper, and res$sims if method is kdejackknifeplus or kdesplitconformal)
                                          try_res <- try(score(res, y_test), silent = FALSE)
                                          if(inherits(try_res, "try-error")) stop("no method for probalistic prediction") else return(try_res)                                          
                                                                            
                                                      }))


# 2 - BaseClassifier -----------------------------------------------------------

BaseClassifier <- R6::R6Class(classname = "BaseClassifier",
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
                                                      params = NULL
                                ) {
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
                                  private$encoded_factors <- encode_factors(y)
                                  private$class_names <- as.character(levels(unique(y)))
                                  Y <- one_hot(y)
                                  self$X_train <- X
                                  self$y_train <- Y
                                  self$params <- list(...)
                                  self$set_model(stats::.lm.fit(x = self$X_train,
                                                                y = self$y_train,
                                                                ...))
                                  self$set_engine(list(fit = stats::.lm.fit, 
                                                       predict = function(obj, X) drop(X%*%obj$coefficients)))
                                  return(base::invisible(self))
                                },
                                predict_proba = function(X, ...) {
                                  if (is.null(self$model) || is.null(self$engine))
                                    stop(paste0(self$name, " must be fitted first"))
                                  raw_preds <- expit(self$engine$predict(self$model, X))
                                  probs <- raw_preds/rowSums(raw_preds)
                                  colnames(probs) <- private$class_names
                                  return(probs)
                                },
                                predict = function(X, ...) {
                                  probs <- self$predict_proba(X, ...)
                                  numeric_factor <- apply(probs, 1, which.max)
                                  res <- decode_factors(numeric_factor, private$encoded_factors)
                                  names(res) <- NULL
                                  return(res)
                                }
                              ))
