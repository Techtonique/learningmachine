

# 1 - RangerRegressor
# -------------------------------------------------------------------

RangerRegressor <- R6::R6Class(classname = "RangerRegressor", inherit = learningmachine::BaseRegressor,
    public = list(name = "RangerRegressor", type = "regression", model = NULL, X_train = NULL,
        y_train = NULL, engine = NULL, params = NULL, initialize = function(name = "RangerRegressor",
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
            if (is_package_available("ranger") == FALSE) install.packages("ranger",
                repos = c(CRAN = "https://cloud.r-project.org"))
            self$X_train <- X
            self$y_train <- y
            self$params <- list(...)
            self$set_model(fit_func_ranger_regression(x = self$X_train, y = self$y_train,
                ...))
            self$set_engine(list(fit = function(x, y) fit_func_ranger_regression(x,
                y, ...), predict = predict_func_ranger))
            return(base::invisible(self))
        }, predict = function(X, level = NULL, method = c("splitconformal", "jackknifeplus",
            "kdesplitconformal", "kdejackknifeplus"), ...) {
            method <- match.arg(method)
            super$predict(X = X, level = level, method = method)
        }))


# 2 - RangerClassifier
# -------------------------------------------------------------------

RangerClassifier <- R6::R6Class(classname = "RangerClassifier", inherit = learningmachine::BaseClassifier,
    public = list(initialize = function(name = "RangerClassifier", type = "classification",
        engine = NULL) {
        self$name <- name
        self$type <- type
        self$engine <- engine
    }, fit = function(X, y, ...) {
        if (is_package_available("ranger") == FALSE) install.packages("ranger", repos = c(CRAN = "https://cloud.r-project.org"))
        stopifnot(is.factor(y))
        private$encoded_factors <- encode_factors(y)
        private$class_names <- as.character(levels(unique(y)))
        self$X_train <- X
        self$y_train <- y
        self$params <- list(...)
        self$set_model(fit_func_ranger_classification(x = self$X_train, y = self$y_train,
            ...))
        self$set_engine(list(fit = function(x, y) fit_func_ranger_classification(x,
            y, ...), predict = function(obj, X) predict_func_ranger(obj, X, type = "response")))
        return(base::invisible(self))
    }, predict_proba = function(X, ...) {
        super$predict_proba(X = X, ...)
    }, predict = function(X, ...) {
        super$predict(X = X, ...)
    }))

# 3 - utils -------------------------------------------------------------------

fit_func_ranger_regression <- function(x, y, ...) {
    df <- data.frame(y = y, x)  # naming of columns is mandatory for `predict`
    ranger::ranger(y ~ ., data = df, ...)
}

fit_func_ranger_classification <- function(x, y, ...) {
    df <- data.frame(y = y, x)  # naming of columns is mandatory for `predict`
    ranger::ranger(y ~ ., data = df, probability = TRUE, ...)
}

predict_func_ranger <- function(obj, newx, ...) {
    if (is.null(colnames(newx)))
        colnames(newx) <- paste0("X", 1:ncol(newx))  # mandatory, linked to df in fit_func

    res <- try(predict(object = obj, data = newx, ...)$predictions, silent = TRUE)  # only accepts a named newx
    if (inherits(res, "try-error")) {
        res <- try(predict(object = obj, data = matrix(newx, nrow = 1), ...)$predictions,
            silent = TRUE)
    }
    return(res)
}
