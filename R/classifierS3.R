#' Create a classifier object for Probabilistic Machine Learning 
#' 
#' @param x Input matrix or data frame of features
#' @param y Vector of target values
#' @param model Model to use for classification
#' @param pi_method Method to use for conformal prediction
#' @param level Confidence level for conformal prediction
#' @param B Number of simulations for conformal prediction
#' @param nb_hidden Number of nodes in the hidden layer
#' @param nodes_sim Type of simulations for hidden nodes
#' @param activ Activation function for hidden layer
#' @param engine Engine to use for fitting the model
#' @param params Additional parameters passed to the model
#' @param seed Reproducibility seed for randomization
#' @param ... Additional arguments passed to Classifier$new()
#' @return A classifier object of class "classifier"
#' @export
#' @examples
#'
#'set.seed(43)
#'X <- as.matrix(iris[, 1:4])
#'y <- iris$Species
#'
#'index_train <- base::sample.int(n = nrow(X),
#'                                size = floor(0.8*nrow(X)),
#'                                replace = FALSE)
#'X_train <- X[index_train, ]
#'y_train <- y[index_train]
#'X_test <- X[-index_train, ]
#'y_test <- y[-index_train]
#'dim(X_train)
#'dim(X_test)
#'
#'obj <- classifier(X_train, y_train)
#'
#'preds <- predict(obj, X_test) 
#' 
classifier <- function(x, y, model = c('ranger', 
       'extratrees', 'ridge', 'bcn', 'glmnet', 'krr', 
       'xgboost', 'svm'), pi_method = c('none', 'splitconformal', 
       'jackknifeplus', 'kdesplitconformal', 'bootsplitconformal', 
       'kdejackknifeplus', 'bootjackknifeplus', 
       'surrsplitconformal', 'surrjackknifeplus'), 
       level = 95,
      B = 100,
      nb_hidden = 0,
      nodes_sim = c("sobol", "halton", "unif"),
      activ = c("relu", "sigmoid", "tanh",
                "leakyrelu", "elu", "linear"),
      engine = NULL,
      params = NULL,
      seed = 123) {

        model <- match.arg(model)
        pi_method <- match.arg(pi_method)
        activ <- match.arg(activ)
        nodes_sim <- match.arg(nodes_sim)

        clf <- Classifier$new(
            model = model,
            pi_method = pi_method,
            level = level,
            B = B,
            nb_hidden = nb_hidden,
            nodes_sim = nodes_sim,
            activ = activ,
            engine = engine,
            params = params,
            seed = seed
        )

        clf$fit(x, y)

        structure(
            list(
                classifier = clf,
                x = x,
                y = y
            ),
            class = "classifier"
        )
}

predict.classifier <- function(object, newx, ...) {
  if (is.data.frame(newx)) {
    newx <- as.matrix(newx)
  }
  # Perform prediction using the classifier object
  object$classifier$predict(newx, ...)
}