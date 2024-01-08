

# 1 - GlmnetRegressor
# -------------------------------------------------------------------

GlmnetRegressor <- R6::R6Class(classname = "GlmnetRegressor", inherit = learningmachine::BaseRegressor,
    public = list(initialize = function(name = "GlmnetRegressor", type = "regression") {
        self$name <- name
        self$type <- type
    }, fit = function(X, y, ...) {
        if (is_package_available("glmnet") == FALSE) install.packages("glmnet", repos = c(CRAN = "https://cloud.r-project.org"))
        self$X_train <- X
        self$y_train <- y
        self$params <- list(...)
        stopifnot(!is.null(self$params$family))
        stopifnot(self$params$family %in% c("gaussian", "poisson", "cox", "mgaussian"))
        self$set_model(glmnet::glmnet(x = self$X_train, y = self$y_train, ...))
        self$set_engine(list(fit = function(x, y) glmnet::glmnet(x, y, ...), predict = predict))
        return(base::invisible(self))
    }, predict = function(X, level = NULL, method = c("splitconformal", "jackknifeplus",
        "other"), ...) {
        method <- match.arg(method)
        super$predict(X = X, level = level, method = method)
    }))

# 2 - GlmnetClassifier
# -------------------------------------------------------------------

GlmnetClassifier <- R6::R6Class(classname = "GlmnetClassifier", inherit = learningmachine::BaseClassifier,
    public = list(initialize = function(name = "GlmnetClassifier", type = "classification") {
        self$name <- name
        self$type <- type
    }, fit = function(X, y, ...) {
        if (is_package_available("glmnet") == FALSE) install.packages("glmnet", repos = c(CRAN = "https://cloud.r-project.org"))
        self$X_train <- X
        self$y_train <- y
        self$params <- list(...)
        stopifnot(!is.null(self$params$family))
        stopifnot(self$params$family %in% c("binomial", "multinomial"))
        self$set_model(glmnet::glmnet(x = self$X_train, y = self$y_train, ...))
        self$set_engine(list(fit = function(x, y) glmnet::glmnet(x, y, ...), predict = predict))

        return(base::invisible(self))
    }, predict_proba = function(X, ...) {
        super$predict_proba(X = X, ...)
    }, predict = function(X, ...) {
        super$predict(X = X, ...)
    }))

# 3 - utils -------------------------------------------------------------------
