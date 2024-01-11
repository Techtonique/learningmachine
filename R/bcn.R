
# 1 - BcnRegressor
# -------------------------------------------------------------------

BcnRegressor <- R6::R6Class(classname = "BcnRegressor", inherit = learningmachine::BaseRegressor,
    public = list(name = "BcnRegressor", type = "regression", model = NULL, X_train = NULL,
        y_train = NULL, engine = NULL, params = NULL, initialize = function(name = "BcnRegressor",
            type = "regression", model = NULL, X_train = NULL, y_train = NULL, engine = NULL,
            params = NULL) {
            self$name <- name
            self$type <- type
            self$model <- model
            self$X_train <- X_train
            self$y_train <- y_train
            self$engine <- engine
            self$params <- params
        }, fit = function(X, y, ...) {

            if (is_package_available("dfoptim") == FALSE) utils::install.packages("dfoptim",
                repos = c(CRAN = "https://cloud.r-project.org"))

            if (is_package_available("bcn") == FALSE) utils::install.packages("bcn",
                repos = c(techtonique = "https://techtonique.r-universe.dev"))

            stopifnot(is.numeric(y))

            self$X_train <- X
            self$y_train <- y
            self$params <- list(...)
            self$set_model(bcn::bcn(x = self$X_train, y = self$y_train, ...))
            self$set_engine(list(fit = function(x, y) bcn::bcn(x, y, ...), predict = bcn::predict.bcn))
            return(base::invisible(self))
        }, predict = function(X, level = NULL, method = c("splitconformal", "jackknifeplus",
            "kdesplitconformal"), ...) {
            method <- match.arg(method)
            super$predict(X = X, level = level, method = method)
        }))

# 2 - BcnClassifier
# -------------------------------------------------------------------

BcnClassifier <- R6::R6Class(classname = "BcnClassifier", inherit = learningmachine::BaseClassifier,
    public = list(name = "BcnClassifier", type = "classification", model = NULL,
        X_train = NULL, y_train = NULL, engine = NULL, params = NULL, initialize = function(name = "BcnClassifier",
            type = "classification", model = NULL, X_train = NULL, y_train = NULL,
            engine = NULL, params = NULL) {
            self$name <- name
            self$type <- type
            self$model <- model
            self$X_train <- X_train
            self$y_train <- y_train
            self$engine <- engine
            self$params <- params
        }, fit = function(X, y, ...) {

            if (is_package_available("dfoptim") == FALSE) utils::install.packages("dfoptim",
                repos = c(CRAN = "https://cloud.r-project.org"))

            if (is_package_available("bcn") == FALSE) utils::install.packages("bcn",
                repos = c(techtonique = "https://techtonique.r-universe.dev"))

            stopifnot(is.factor(y))
            private$encoded_factors <- encode_factors(y)
            private$class_names <- as.character(levels(unique(y)))
            self$X_train <- X
            self$y_train <- y
            self$params <- list(...)
            self$set_model(bcn::bcn(x = self$X_train, y = self$y_train, ...))
            self$set_engine(list(fit = function(x, y) bcn::bcn(x, y, ...), predict = function(obj,
                X) bcn::predict.bcn(obj, X, type = "probs")))
            return(base::invisible(self))
        }, predict_proba = function(X, ...) {
            super$predict_proba(X = X, ...)
        }, predict = function(X, ...) {
            super$predict(X = X, ...)
        }))

# 3 - utils -------------------------------------------------------------------

