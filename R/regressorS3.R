#' Create a regressor object for Probabilistic Machine Learning 
#' 
#' @param x Input matrix or data frame of features
#' @param y Vector of target values
#' @param model Model to use for regression
#' @param pi_method Method to use for conformal prediction
#' @param level Confidence level for conformal prediction
#' @param B Number of simulations for conformal prediction
#' @param nb_hidden Number of nodes in the hidden layer
#' @param nodes_sim Type of simulations for hidden nodes
#' @param activ Activation function for hidden layer
#' @param engine Engine to use for fitting the model
#' @param params Additional parameters passed to the model
#' @param seed Reproducibility seed for randomization
#' @param type_split Type of data splitting for split conformal prediction: 
#' "stratify" (for classical supervised learning) "sequential" (when 
#' the data sequential ordering matters)
#' @param ... Additional arguments passed to Regressor$new()
#' @return A regressor object of class "regressor"
#' @export
#' @examples
#' 
#' X <- mtcars[, -1]
#' y <- mtcars$mpg
#' X_train <- X[1:25, ]
#' y_train <- y[1:25]
#' X_test <- X[26:32, ]
#' y_test <- y[26:32]
#' reg <- regressor(X_train, y_train, pi_method = "splitconformal", level = 95)
#' 
#' print(sqrt(mean((y_test - predict(reg, newx = X_test))^2)))
#' 
regressor <- function(x, y, model = c('ranger', 
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
      type_split = c("stratify", "sequential"),
      seed = 123) {
  # Convert data frame to matrix if needed
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  model <- match.arg(model)
  pi_method <- match.arg(pi_method)
  nodes_sim <- match.arg(nodes_sim)
  activ <- match.arg(activ)  
  type_split <- match.arg(type_split)
  
  # Create R6 Regressor object
  reg <- Regressor$new(
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
  
  # Fit the model
  reg$fit(x, y, type_split = type_split)
  
  # Convert to S3 object
  structure(
    list(
      regressor = reg,
      x = x,
      y = y
    ),
    class = "regressor"
  )
}

#' Predict using a regressor object
#' 
#' @param object A regressor object
#' @param newx Matrix or data frame of new observations
#' @param ... Additional arguments (not used)
#' @return Vector of predicted values
#' @export
predict.regressor <- function(object, newx, ...) {
  if (is.data.frame(newx)) {
    newx <- as.matrix(newx)
  }
  object$regressor$predict(newx, ...)
}