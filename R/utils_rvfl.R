
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
  debug_print(input_list)
  base::stopifnot(length(reg_lambda) == 1)
  out <- do.call(bayesianrvfl::fit_rvfl, c(list(x = x, 
                                y = y, 
                                lambda = reg_lambda,
                                compute_Sigma = FALSE, 
                                method = "chol"),
                                input_list))
  
  return(structure(out, class = "rvfl"))
}

predict_rvfl_regression <- function(object, newx, ...)
{
  preds <- try(bayesianrvfl::predict_rvfl(fit_obj = object, 
                             newx = newx, 
                             ...)$mean, silent = TRUE) # NOT clean, instead, must fix compute_Sigma in bayesianrvfl
  if (inherits(preds, "try-error"))
  {
    preds <- bayesianrvfl::predict_rvfl(fit_obj = object, 
                                            newx = newx, 
                                            ...)
  }
  return (preds)
}

update_rvfl_regressor <- function(object, newx, newy, method="rvfl")
{
  bayesianrvfl::update_params(fit_obj=object, newx=newx, newy=newy)
}
