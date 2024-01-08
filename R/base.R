

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
                                                             "bootsplitconformal",
                                                             "kdesplitconformal"),
                                                  B = 10,  
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
                                   
                                   if (identical(method, "jackknifeplus"))
                                   {
                                     stopifnot(!is.null(self$X_train))
                                     stopifnot(!is.null(self$y_train))
                                     n_train <- nrow(self$X_train)
                                     fit_objs_loocv <- base::vector("list", n_train)
                                     residuals_loocv <- rep(0, n_train)
                                     
                                     pb <- txtProgressBar(min = 1, max = n_train, style = 3)
                                     for (i in 1:n_train) {
                                       left_out_indices <- setdiff(1:n_train, i)
                                       residuals_loocv[i] <-
                                         get_jackknife_residuals(X = self$X_train, 
                                                                 y = self$y_train, 
                                                                 idx = left_out_indices, 
                                                                 fit_func = self$engine$fit,
                                                                 predict_func = self$engine$predict)
                                       setTxtProgressBar(pb, i)
                                     }
                                     close(pb)
                                     
                                     quantile_absolute_residuals <- quantile(residuals_loocv, 
                                                                             level/100)
                                     
                                     preds <- self$engine$predict(self$model, X, ...)
                                    
                                     return(list(preds = preds,
                                                 lower = preds - quantile_absolute_residuals,
                                                 upper = preds + quantile_absolute_residuals))
                                   }
                                   
                                   if (method %in% c("splitconformal", "bootsplitconformal"))
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

                                     if (identical(method, "bootsplitconformal"))
                                    {
                                      stopifnot(!is.null(B) && is.numeric(B))                                      
                                      resampled_preds <- sapply(1:floor(B), function(i) {set.seed(self$seed+i*100); preds + base::sample(abs_residuals, 
                                      size=length(preds), replace=TRUE)})
                                      stopifnot((dim(resampled_preds)[1] == length(preds)) && (dim(resampled_preds)[2] == self$B))
                                      preds_upper <- apply(resampled_preds, 1, function(x) quantile(x, probs = 1 - (1 - level / 100) / 2))
                                      preds_lower <- apply(resampled_preds, 1, function(x) quantile(x, probs = (1 - level / 100) / 2))
                                      
                                      return(list(preds = preds,
                                                  sims = resampled_preds,
                                                  lower = preds_lower,
                                                  upper = preds_upper))
                                    }                                                                               
                                     
                                   }                                                                      
                                   
                                 }
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
