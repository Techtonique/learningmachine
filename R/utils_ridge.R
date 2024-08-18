
fit_ridge_regression <- function(x, y, 
                                 ...)
{
  # adapted from MASS::lm.ridge
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
  nreg_lambda <- 1
  
  ym <- mean(y)
  centered_y <- y - ym
  
  x_scaled <- base::scale(x)
  attrs <- attributes(x_scaled)
  X <- as.matrix(x_scaled[, ])
  
  Xs <- La.svd(X)
  rhs <- crossprod(Xs$u, centered_y)
  d <- Xs$d
  nb_di <- length(d)
  div <- d ^ 2 + rep(reg_lambda, rep(nb_di, nreg_lambda))
  a <- drop(d * rhs) / div
  dim(a) <- c(nb_di, nreg_lambda)
  n <- nrow(X)
  
  coef <- crossprod(Xs$vt, a)
  colnames(coef) <- reg_lambda
  centered_y_hat <- X %*% coef
  
  fitted_values <- drop(ym +  centered_y_hat)
  if (length(reg_lambda) > 1)
  {
    colnames(fitted_values) <- reg_lambda
  }
  residuals <- centered_y - centered_y_hat
  GCV <-
    colSums(residuals ^ 2) / (n - colSums(matrix(d ^ 2 / div, nb_di))) ^ 2
  BIC <- n * log(colMeans(residuals ^ 2)) + (ncol(X) + 2) * log(n)
  
  out <- list(
    coef = drop(coef),
    ym = ym,
    xm = attrs$`scaled:center`,
    xsd = attrs$`scaled:scale`,
    reg_lambda = reg_lambda,
    best_lam = reg_lambda[which.min(GCV)],
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
  if (length(object$reg_lambda) > 1)
  {
    res <- try(drop(
      base::scale(newx, center = object$xm,
                  scale = object$xsd) %*% object$coef[, which.min(object$GCV)] + object$ym
    ),
    silent = TRUE)
    # if (inherits(res, "try-error"))
    # {
    #   res <- try(drop(
    #     base::scale(newx, center = object$xm,
    #                 scale = object$xsd) %*% object$coef[which.min(object$GCV)] + object$ym
    #   ),
    #   silent = TRUE)
    #   return(res)
    # } else {
       return(res)
    #}
  }  else {
    return(drop(
      base::scale(newx, center = object$xm,
                  scale = object$xsd) %*% object$coef + object$ym
    ))
  }
}

predict_ridge_multitaskregression <- function(objects, newx)
{
  preds <- matrix(NA, nrow = nrow(newx), ncol = length(objects))
  j <- 1
  for (object in objects)
  {
    if (length(object$reg_lambda) > 1)
    {
      res <- try(drop(
        base::scale(newx, center = object$xm,
                    scale = object$xsd) %*% object$coef[, which.min(object$GCV)] + object$ym
      ),
      silent = TRUE)
      if (inherits(res, "try-error"))
      {
        res <- try(drop(
          base::scale(newx, center = object$xm,
                      scale = object$xsd) %*% object$coef[which.min(object$GCV)] + object$ym
        ),
        silent = TRUE)
        preds[, j] <- res
      } else {
        preds[, j] <- res
      }
    }  else {
      # length(object$reg_lambda) <= 1
      preds[, j] <- drop(
        base::scale(newx, center = object$xm,
                    scale = object$xsd) %*% object$coef + object$ym
      )
    }
    j <- j + 1
  }
  return(preds)
}