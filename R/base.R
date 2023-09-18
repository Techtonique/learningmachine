

# 1 - BaseRegressor -----------------------------------------------------------

BaseRegressor <- R6::R6Class("BaseRegressor",
                             public = list(
                               name = "BaseRegressor",
                               type = "regression",
                               model = NULL,
                               X_train = NULL,
                               y_train = NULL,
                               initialize = function(name = "BaseRegressor",
                                                     type = "regression",
                                                     model = NULL,
                                                     X_train = NULL,
                                                     y_train = NULL) {
                                 self$name <- name
                                 self$type <- type
                                 self$model <- model
                                 self$X_train <- X_train
                                 self$y_train <- y_train
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
                               fit = function(X, y, ...) {
                                 self$X_train <- X
                                 self$y_train <- y
                                 self$set_model(stats::.lm.fit(x = self$X_train,
                                                               y = self$y_train,
                                                               ...))
                                 return(base::invisible(self))
                               },
                               predict = function(X, level = NULL,
                                                  method = c("splitconformal",
                                                             "other"),
                                                  ...) {
                                 if (is.null(level))
                                 {

                                   return(drop(X%*%self$model$coefficients))

                                 } else { # prediction interval

                                   method <- match.arg(method)

                                   if (identical(method, "splitconformal"))
                                   {
                                     # see https://github.com/ryantibs/conformal
                                     # see https://github.com/ryantibs/conformal
                                     # see https://github.com/ryantibs/conformal
                                     stopifnot(is_wholenumber(level))
                                     stopifnot(level > 0 && level < 100)
                                     stopifnot(!is.null(self$X_train))
                                     stopifnot(!is.null(self$y_train))

                                     idx_train_calibration <- split_data(self$y_train,
                                                                         p = 0.5,
                                                                         seed = 123)

                                     X_train_sc <- self$X_train[idx_train_calibration,]
                                     y_train_sc <- self$y_train[idx_train_calibration]
                                     X_calibration_sc <- self$X_train[-idx_train_calibration,]
                                     y_calibration_sc <- self$y_train[-idx_train_calibration]

                                     fit_obj_train_sc <- .lm.fit(X_train_sc, y_train_sc)
                                     y_pred_calibration <- X_calibration_sc%*%fit_obj_train_sc$coefficients
                                     abs_residuals <- abs(y_calibration_sc - y_pred_calibration)

                                     # quantile_absolute_residuals_lower <- quantile_scp(abs_residuals,
                                     #                                                   alpha = (1 - level / 100) / 2)
                                     # quantile_absolute_residuals_upper <- quantile_scp(abs_residuals,
                                     #                                                   alpha = 1 - (1 - level / 100) / 2)

                                     # quantile_absolute_residuals_lower <- quantile(abs_residuals,
                                     #                                               probs = (1 - level / 100) / 2)
                                     # quantile_absolute_residuals_upper <- quantile(abs_residuals,
                                     #                                               probs = 1 - (1 - level / 100) / 2)

                                     quantile_absolute_residuals_lower <- quantile_scp(abs_residuals,
                                                                                       alpha = (1 - level / 100))
                                     quantile_absolute_residuals_upper <- quantile_scp(abs_residuals,
                                                                                       alpha = (1 - level / 100))

                                     preds <- drop(X%*%self$model$coefficients)

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
                                                  model = NULL) {
                              self$name <- name
                              self$type <- type
                              self$model <- model
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
                            }))
