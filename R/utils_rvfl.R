
fit_rvfl_regression <- function(x, y,
                                 ...)
{
  x <- as.matrix(x)
  y <- as.vector(y)
  input_list <- list(...)
  if ("reg_lambda" %in% names(input_list))
  {
    reg_lambda <- input_list$reg_lambda
    input_list <- input_list[names(input_list) != "reg_lambda"]
  } else {
    reg_lambda <- 0.01
  }
  base::stopifnot(length(reg_lambda) == 1)
  out <- do.call(bayesianrvfl::fit_rvfl, c(list(x = x, 
                                y = y, 
                                lambda = reg_lambda,
                                compute_Sigma = FALSE, 
                                method = "chol"),
                                input_list))
  return(structure(out, class = "rvfl"))
}

fit_bayesianrvfl_regression <- function(x, y,
                                 ...)
{
  x <- as.matrix(x)
  y <- as.vector(y)
  input_list <- list(...)
  if ("reg_lambda" %in% names(input_list))
  {
    reg_lambda <- input_list$reg_lambda
    input_list <- input_list[names(input_list) != "reg_lambda"]
  } else {
    reg_lambda <- 0.01
  }
  base::stopifnot(length(reg_lambda) == 1)
  out <- do.call(bayesianrvfl::fit_rvfl, c(list(x = x, 
                                y = y, 
                                lambda = reg_lambda,
                                compute_Sigma = TRUE, 
                                method = "chol"),
                                input_list))
  return(structure(out, class = "rvfl"))
}

predict_rvfl_regression <- function(object, newx, ...)
{
  return (bayesianrvfl::predict_rvfl(fit_obj = object, 
                                            newx = newx, 
                                            ...))
}

predict_bayesianrvfl_regression <- function(object, newx, level = 95, ...)
{
  res <- bayesianrvfl::predict_rvfl(fit_obj = object, 
                                    newx = newx, 
                                    ...)
  multiplier <- qnorm(1 - (1 - level / 100) / 2)
  return(list(
  preds = res$mean,  
  lower = res$mean - multiplier * res$sd,
  upper = res$mean + multiplier * res$sd,
  simulate = res$simulate
))
}

update_rvfl_regressor <- function(object, newx, newy, ...)
{
  bayesianrvfl::update_params(fit_obj=object, newx=newx, newy=newy, ...)
}
