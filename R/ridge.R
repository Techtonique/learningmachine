

# 1 - AutoRidgeRegressor
# -------------------------------------------------------------------

AutoRidgeRegressor <- R6::R6Class(classname = "AutoRidgeRegressor", inherit = learningmachine::BaseRegressor,
    public = list(name = "AutoRidgeRegressor", type = "regression", model = NULL, X_train = NULL,
        y_train = NULL, engine = NULL, params = NULL, initialize = function(name = "AutoRidgeRegressor",
            type = "regression", model = NULL, X_train = NULL, y_train = NULL, engine = NULL,
            params = NULL) {
            self$name <- name
            self$type <- type
            self$model <- model
            self$X_train <- X_train
            self$y_train <- y_train
            self$engine <- engine
            self$params <- params
        }, fit = function(X, y) {            
            self$X_train <- X
            self$y_train <- y
            self$params <- NULL #list(...)
            self$set_model(fit_ridge_regression(x = self$X_train, y = self$y_train))
            self$set_engine(list(fit = function(x, y) fit_ridge_regression(x, y), 
            predict = predict_ridge_regression))
            return(base::invisible(self))
        }, predict = function(X, level = NULL, method = c("splitconformal", "jackknifeplus",
            "kdesplitconformal", "kdejackknifeplus"), B=100) {
            method <- match.arg(method)
            super$predict(X = X, level = level, method = method)
        }))


# 2 - AutoRidgeClassifier
# -------------------------------------------------------------------

AutoRidgeClassifier <- R6::R6Class(classname = "AutoRidgeClassifier", inherit = learningmachine::BaseClassifier,
    public = list(initialize = function(name = "AutoRidgeClassifier", type = "classification",
        engine = NULL) {
        self$name <- name
        self$type <- type
        self$engine <- engine
    }, fit = function(X, y, ...) {
        stopifnot(is.factor(y))
        private$encoded_factors <- encode_factors(y)
        private$class_names <- as.character(levels(unique(y)))
        Y <- one_hot(y)
        self$X_train <- X
        self$y_train <- y
        self$params <- list(...)        
        
        fit_objs <- lapply(1:ncol(Y), function(j) {
            fit_ridge_regression(x = self$X_train, y = Y[, j])
        })            
        
        self$set_model(fit_objs)
        self$set_engine(list(fit = fit_ridge_regression,
        predict = function(objs, X) predict_ridge_multitaskregression(objs, X)))
        return(base::invisible(self))
    }, predict_proba = function(X, ...) {
        super$predict_proba(X = X, ...)
    }, predict = function(X, ...) {
        super$predict(X = X, ...)
    }))

# 3 - utils -------------------------------------------------------------------

fit_ridge_regression <- function(x, y, lambda=10^seq(-10, 10,
                                          length.out = 100))
{
  # adapted from MASS::lm.ridge
  x <- as.matrix(x)
  y <- as.vector(y)
  nlambda <- length(lambda)

  ym <- mean(y)
  centered_y <- y - ym

  x_scaled <- base::scale(x)
  attrs <- attributes(x_scaled)
  X <- as.matrix(x_scaled[,])

  Xs <- La.svd(X)
  rhs <- crossprod(Xs$u, centered_y)
  d <- Xs$d
  nb_di <- length(d)
  div <- d ^ 2 + rep(lambda, rep(nb_di, nlambda))
  a <- drop(d * rhs) / div
  dim(a) <- c(nb_di, nlambda)
  n <- nrow(X)

  coef <- crossprod(Xs$vt, a)
  colnames(coef) <- lambda
  centered_y_hat <- X %*% coef

  fitted_values <- drop(ym +  centered_y_hat)
  if (length(lambda) > 1)
  {
    colnames(fitted_values) <- lambda
  }
  residuals <- centered_y - centered_y_hat
  GCV <- colSums(residuals^2)/(n - colSums(matrix(d^2/div, nb_di)))^2
  BIC <- n*log(colMeans(residuals^2)) + (ncol(X) + 2)*log(n)

  out <- list(
      coef = drop(coef),
      ym = ym,
      xm = attrs$`scaled:center`,
      xsd = attrs$`scaled:scale`,
      lambda = lambda,
      best_lam = lambda[which.min(GCV)],
      fitted_values = fitted_values,
      residuals = drop(centered_y - centered_y_hat),
      GCV = GCV,
      BIC = BIC,
      x = x,
      y = y
    )

  return(structure(out, class = "ridge"))
}

predict_ridge_regression <- function(object, newx)
{
  if (length(object$lambda) > 1)
  {
    res <- try(drop(base::scale(newx, center=object$xm,
                                scale=object$xsd)%*%object$coef[,which.min(object$GCV)] + object$ym),
               silent = TRUE)
    if (inherits(res, "try-error"))
    {
      res <- try(drop(base::scale(newx, center=object$xm,
                                  scale=object$xsd)%*%object$coef[which.min(object$GCV)] + object$ym),
                 silent = TRUE)
      return(res)
    } else {
      return(res)
    }
  }  else {
    return(drop(base::scale(newx, center=object$xm,
                            scale=object$xsd)%*%object$coef + object$ym))
  }
}

predict_ridge_multitaskregression <- function(objects, newx)
{
  preds <- matrix(NA, nrow = nrow(newx), ncol = length(objects))
  j <- 1
  for (object in objects)
  {
    if (length(object$lambda) > 1)
    {
      res <- try(drop(base::scale(newx, center=object$xm,
                                  scale=object$xsd)%*%object$coef[,which.min(object$GCV)] + object$ym),
                silent = TRUE)
      if (inherits(res, "try-error"))
      {
        res <- try(drop(base::scale(newx, center=object$xm,
                                    scale=object$xsd)%*%object$coef[which.min(object$GCV)] + object$ym),
                  silent = TRUE)
        preds[, j] <- res        
      } else {      
        preds[, j] <- res
      }
    }  else { # length(object$lambda) <= 1
      preds[, j] <- drop(base::scale(newx, center=object$xm,
                              scale=object$xsd)%*%object$coef + object$ym)
    }
    j <- j + 1
  }
  return(preds)
}