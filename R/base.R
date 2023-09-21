

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
                               initialize = function(name = "BaseRegressor",
                                                     type = "regression",
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
                               fit = function(X, y, ...) {
                                 self$X_train <- X
                                 self$y_train <- y
                                 self$params <- list(...)
                                 self$set_model(stats::.lm.fit(x = self$X_train,
                                                               y = self$y_train,
                                                               ...))
                                 self$set_engine(list(fit = stats::.lm.fit, 
                                                      predict = function(X) drop(X%*%self$model$coefficients)))
                                 return(base::invisible(self))
                               },
                               predict = function(X, level = NULL,
                                                  method = c("splitconformal",
                                                             "other"),
                                                  ...) {
                                 
                                 if (is.null(self$model) || is.null(self$engine))
                                  stop(paste0(self$name, " must be fitted first"))
                                 
                                 if (is.null(level)) # no prediction interval
                                 {
                                   try_return <- try(return(self$engine$predict(X)), 
                                       silent = TRUE)
                                   if (inherits(try_return, "try-error"))
                                   {
                                     return(self$engine$predict(self$model, X))
                                   }
                                 } else { # prediction interval

                                   method <- match.arg(method)

                                   if (identical(method, "splitconformal"))
                                   {
                                     stopifnot(is_wholenumber(level))
                                     stopifnot(level > 0 && level < 100)
                                     stopifnot(!is.null(self$X_train))
                                     stopifnot(!is.null(self$y_train))

                                     idx_train_calibration <- split_data(self$y_train,
                                                                         p = 0.5,
                                                                         seed = 123)

                                     X_train_sc <- self$X_train[idx_train_calibration, ]
                                     y_train_sc <- self$y_train[idx_train_calibration]
                                     X_calibration_sc <- self$X_train[-idx_train_calibration, ]
                                     y_calibration_sc <- self$y_train[-idx_train_calibration]
                                      
                                     fit_obj_train_sc <- self$engine$fit(x = X_train_sc, y = y_train_sc)
                                     y_pred_calibration <- try(self$engine$predict(X = X_calibration_sc), 
                                                               silent = TRUE)
                                     if (inherits(y_pred_calibration, "try-error"))
                                     {
                                       y_pred_calibration <- self$engine$predict(self$model, X_calibration_sc)
                                     }
                                     abs_residuals <- abs(y_calibration_sc - y_pred_calibration)

                                     quantile_absolute_residuals_lower <- quantile_scp(abs_residuals,
                                                                                       alpha = (1 - level / 100))
                                     quantile_absolute_residuals_upper <- quantile_scp(abs_residuals,
                                                                                       alpha = (1 - level / 100))
                                     
                                     preds <- try(return(self$engine$predict(X, ...)), 
                                                       silent = TRUE)
                                     if (inherits(preds, "try-error"))
                                     {
                                       preds <- self$engine$predict(self$model, X, ...)
                                     }

                                     return(list(preds = preds,
                                                 lower = preds - quantile_absolute_residuals_lower,
                                                 upper = preds + quantile_absolute_residuals_upper))
                                   }

                                   if (identical(method, "other"))
                                   {

                                   }

                                 }
                               }))


# 2 - BaseClassifier -----------------------------------------------------------

BaseClassifier <- R6::R6Class(classname = "BaseClassifier",
                          public = list(
                            name = "BaseClassifier",
                            type = "classification",
                            model = NULL,
                            initialize = function(name = "BaseClassifier",
                                                  type = "classification",
                                                  model = NULL, 
                                                  engine = NULL) {
                              self$name <- name
                              self$type <- type
                              self$model <- model
                              self$engine <- engine
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
                            }))
