
fit_matern32_regression <-
  function(x,
           y,
           reg_lambda = 0.1, #reg_lambda = 10 ^ seq(-10, 10, length.out = 100),
           l = NULL,
           method = "chol", #method = c("chol", "solve", "svd", "eigen"),
           with_kmeans = FALSE,
           centers = NULL,
           #centering = FALSE,
           seed = 123,
           cl = NULL,
           ...)
  {
    method <- match.arg(method)
    #
    # train_cov = amp*cov_map(exp_quadratic, x) + eye * (noise + 1e-6)
    # chol = scipy.linalg.cholesky(train_cov, lower=True)
    # kinvy = scipy.linalg.solve_triangular(
    #   chol.T, scipy.linalg.solve_triangular(chol, y, lower=True))
    #
    # jax$scipy$linalg$cholesky()
    # jax$scipy$linalg$solve_triangular()
    
    ## regression ----
    x <- as.matrix(x)
    y <- as.vector(y)
    nreg_lambda <- length(reg_lambda)
    n <- dim(x)[1]
    p <- dim(x)[2]
    stopifnot(n == length(y))
    one_reg_lambda <- (length(reg_lambda) <= 1)
    cclust_obj <- NULL
    
    
    # centered response?
    # if (centering)
    # {
    #   ym <- mean(y)
    #   response_y <- y - ym
    # } else {
      ym <- mean(y)
      response_y <- y
    #}
    
    # construct covariance
    x_scaled <- my_scale(x)
    X <- x_scaled$res
    X_clust <- NULL
    
    if (is.null(l))
      l <- sqrt(p)
    
    # compute kernel as a vector if necessary
    if (length(l) == 1)
      l <- rep(l, p)
    
    
    # 1 - compute kernel K -----
    
    # 1 - 1 - K for n > 500 -----
    if (n > 500)
      # can use kmeans
    {
      # 1 - 1 - 1 K with k-means -----
      if (with_kmeans == TRUE)
      {
        # adjust KRR to centers = X and this new response = y
        if (is.null(centers))
        {
          warning("with_kmeans == TRUE but 'centers' not provided: set to 100")
          centers <- 100
          
        }
        #is.null(centers) == FALSE
        # fitted values, residuals, etc => predict on entire dataset from reduced kernel
        
        #set.seed(seed)
        #cclust_obj <- cclust::cclust(x = as.matrix(X),
        #                             centers = centers)
        #X_clust <- as.matrix(cclust_obj$centers)
        
        set.seed(seed)
        cclust_obj <-
          stats::kmeans(x = as.matrix(X), centers = centers)
        X_clust <- as.matrix(cclust_obj$centers)
        
        response_y_clust <- parfor(args=1:centers,
                                   function(i)
                                     median(response_y[which(cclust_obj$cluster == i)]))
        K <- matern32_kxx_cpp(x = X_clust, l = l)
        
        
      } else {
        # 1 - 1 - 2 K without k-means -----
        cat(
          "Processing... (try using option 'with_kmeans' for faster results when nrow(x) > 500)",
          "\n"
        )
        K <- matern32_kxx_cpp(x = X, l = l)
      }
      
    } else {
      # 1 - 2 - K for n <= 500 -----
      if (with_kmeans == TRUE)
      {
        # 1 - 2 - 1 with k-means -----
        stop("option 'with_kmeans' not useful for n_obs <= 500")
      } else {
        # 1 - 2 - 2 without k-means -----
        K <- matern32_kxx_cpp(x = X, l = l)
      }
      
    }
    
    
    # 2 - compute coeffs -----
    
    if (with_kmeans == FALSE)
    {
      # 2 - 1 - without k-means -----
      
      if (method == "solve")
      {
        if (one_reg_lambda)
        {
          K_plus <- K + reg_lambda * diag(dim(K)[1])
          invK <- solve(K_plus)
          coef <- invK %*% response_y
          loocv <- sum(drop(coef / diag(invK)) ^ 2)
        } else {
          # length(reg_lambda) > 1
          
          get_loocv <- function(reg_lambda_i)
          {
            K_plus <- K + reg_lambda_i * diag(dim(K)[1])
            invK <- solve(K_plus)
            coef <- invK %*% response_y
            return(list(coef = coef,
                        loocv = drop(coef / diag(invK))))
          }
          
          fit_res <- lapply(reg_lambda, function(x)
            get_loocv(x))
          n_fit_res <- length(fit_res)
          
          loocv <- colSums(parfor(args=1:n_fit_res,
                                  function(i)
                                    fit_res[[i]]$loocv) ^ 2)
          names(loocv) <- reg_lambda
          
          coefs <- parfor(args=1:n_fit_res,  function(i)
            fit_res[[i]]$coef)
          colnames(coefs) <- reg_lambda
        }
      }
      
      if (method == "svd")
      {
        Xs <- La.svd(K)
        rhs <- crossprod(Xs$u, response_y)
        d <- Xs$d
        nb_di <- length(d)
        div <- d ^ 2 + rep(reg_lambda, rep(nb_di, nreg_lambda))
        a <- drop(d * rhs) / div
        dim(a) <- c(nb_di, nreg_lambda)
        coef <- crossprod(Xs$vt, a)
        colnames(coef) <- reg_lambda
        
        response_y_hat <- K %*% coef
        
        # if (centering)
        # {
        #   fitted_values <- drop(ym +  response_y_hat)
        # } else {
          fitted_values <- drop(response_y_hat)
        #}
        
        resid <- response_y - response_y_hat
        colnames(resid) <- reg_lambda
        GCV <-
          colSums(resid ^ 2) / (nrow(X) - colSums(matrix(d ^ 2 / div,
                                                         nb_di))) ^ 2
        
        if (length(reg_lambda) > 1)
        {
          RSS <- colSums((y - fitted_values) ^ 2)
        } else {
          RSS <- sum((y - fitted_values) ^ 2)
        }
        
        TSS <- sum((y - ym) ^ 2)
        R_Squared <- 1 - RSS / TSS
        names(R_Squared) <- reg_lambda
        Adj_R_Squared <-
          1 - (1 - R_Squared) * ((n - 1) / (n - p - 1))
        
        res <- list(
          K = K,
          l = l,
          reg_lambda = reg_lambda,
          coef = drop(coef),
          #centering = centering,
          scales = x_scaled$xsd,
          ym = ym,
          xm = x_scaled$xm,
          fitted_values = fitted_values,
          resid = resid,
          GCV = GCV,
          R_Squared = R_Squared,
          Adj_R_Squared = Adj_R_Squared,
          scaled_x = X,
          with_kmeans = with_kmeans,
          cclust_obj = cclust_obj,
          scaled_x_clust = X_clust,
          x = x,
          response_y = response_y,
          fit_method = method
        )
        
        class(res) <- "matern32"
        
        return(res)
      }
      
      if (method == "chol")
      {
        if (one_reg_lambda)
        {
          K_plus <- K + reg_lambda * diag(dim(K)[1])
          invK <- chol2inv(chol(K_plus))
          coef <- invK %*% response_y
          loocv <- sum(drop(coef / diag(invK)) ^ 2)
        } else {
          # length(reg_lambda) > 1
          
          get_loocv <- function(reg_lambda_i)
          {
            K_plus <- K + reg_lambda_i * diag(dim(K)[1])
            invK <- chol2inv(chol(K_plus))
            coef <- invK %*% response_y
            return(list(coef = coef,
                        loocv = drop(coef / diag(invK))))
          }
          
          fit_res <- lapply(reg_lambda, function(x)
            get_loocv(x))
          n_fit_res <- length(fit_res)
          
          loocv <- colSums(parfor(args=1:n_fit_res,
                                  what=function(i)
                                    fit_res[[i]]$loocv) ^ 2)
          names(loocv) <- reg_lambda
          
          coefs <- parfor(args=1:n_fit_res, what=function(i)
            fit_res[[i]]$coef)
          colnames(coefs) <- reg_lambda
        }
      }
      
      if (method == "eigen")
      {
        if (one_reg_lambda)
        {
          eigenK <- base::eigen(K)
          eigen_values <- eigenK$values
          Q <- eigenK$vectors
          inv_eigen <- solve_eigen(
            Eigenvectors = Q,
            Eigenvalues = eigen_values,
            y = response_y,
            reg_lambda = reg_lambda
          )
          
          coef <- inv_eigen$coeffs
          loocv <- sum(inv_eigen$loocv ^ 2)
        } else {
          # length(reg_lambda) > 1
          
          get_loocv <- function(reg_lambda_i)
          {
            eigenK <- base::eigen(K)
            eigen_values <- eigenK$values
            Q <- eigenK$vectors
            inv_eigen <- solve_eigen(
              Eigenvectors = Q,
              Eigenvalues = eigen_values,
              y = response_y,
              reg_lambda = reg_lambda_i
            )
            return(list(coef = inv_eigen$coef,
                        loocv = inv_eigen$loocv))
          }
          
          fit_res <- lapply(reg_lambda, function(x)
            get_loocv(x))
          n_fit_res <- length(fit_res)
          
          loocv <- colSums(parfor(args=1:n_fit_res,
                                  what=function(i)
                                    fit_res[[i]]$loocv) ^ 2)
          names(loocv) <- reg_lambda
          
          coefs <- parfor(args=1:n_fit_res, what=function(i)
            fit_res[[i]]$coef)
          colnames(coefs) <- reg_lambda
          
        }
      }
      
    } else {
      # 2 - 2 - with k-means -----
      if (n > 500)
      {
        # 2 - 2 - 1 For n > 500 -----
        
        if (method %in% c("chol", "solve"))
        {
          if (one_reg_lambda)
          {
            K_plus <- K + reg_lambda * diag(dim(K)[1])
            invK <- switch(method,
                           "solve" = solve(K_plus),
                           "chol" = chol2inv(chol(K_plus)))
            coef <- invK %*% response_y_clust
            loocv <- sum(drop(coef / diag(invK)) ^ 2)
          } else {
            # length(reg_lambda) > 1
            
            get_loocv <- function(reg_lambda_i)
            {
              K_plus <- K + reg_lambda_i * diag(dim(K)[1])
              invK <- switch(method,
                             "solve" = solve(K_plus),
                             "chol" = chol2inv(chol(K_plus)))
              coef <- invK %*% response_y_clust
              return(list(coef = coef,
                          loocv = drop(coef / diag(invK))))
            }
            
            fit_res <- lapply(reg_lambda, function(x)
              get_loocv(x))
            n_fit_res <- length(fit_res)
            
            loocv <- colSums(parfor(args=1:n_fit_res,
                                    what=function(i)
                                      fit_res[[i]]$loocv) ^ 2)
            names(loocv) <- reg_lambda
            
            coefs <-
              parfor(args=1:n_fit_res, what=function(i)
                fit_res[[i]]$coef)
            colnames(coefs) <- reg_lambda
          }
        }
        
        if (method == "svd")
        {
          Xs <- La.svd(K) # K is based on clustered X
          rhs <- crossprod(Xs$u, response_y_clust)
          d <- Xs$d
          nb_di <- length(d)
          div <- d ^ 2 + rep(reg_lambda, rep(nb_di, nreg_lambda))
          a <- drop(d * rhs) / div
          dim(a) <- c(nb_di, nreg_lambda)
          coef <- crossprod(Xs$vt, a)
          colnames(coef) <- reg_lambda
          scales <- x_scaled$xsd
          xm <- x_scaled$xm
          
          K_star <-
            matern32_kxstar_cpp(newx = X,
                                # X is already scaled
                                x = X_clust,
                                l = l)
          
          response_y_hat <- K_star %*% coef
          
          # if (centering)
          # {
          #   fitted_values <- drop(ym +  response_y_hat)
          # } else {
            fitted_values <- drop(response_y_hat)
          #}
          
          resid <- response_y - response_y_hat
          colnames(resid) <- reg_lambda
          GCV <-
            colSums(resid ^ 2) / (nrow(X) - colSums(matrix(d ^ 2 / div,
                                                           nb_di))) ^ 2
          
          if (length(reg_lambda) > 1)
          {
            RSS <- colSums((y - fitted_values) ^ 2)
          } else {
            RSS <- sum((y - fitted_values) ^ 2)
          }
          
          TSS <- sum((y - ym) ^ 2)
          R_Squared <- 1 - RSS / TSS
          Adj_R_Squared <-
            1 - (1 - R_Squared) * ((n - 1) / (n - p - 1))
          names(R_Squared) <- reg_lambda
          
        }
        
      } else {
        # 2 - 2 - 2 For n <= 500 -----
        stop("option 'with_kmeans' not implemented for n_obs <= 500")
        
      }
      
    }
    
    
    # 3 - returns -----
    
    
    # 3 - 1 svd -----
    if (method == "svd")
    {
      res <- list(
        K = K,
        l = l,
        reg_lambda = reg_lambda,
        coef = drop(coef),
        #centering = centering,
        scales = scales,
        ym = ym,
        xm = xm,
        fitted_values = fitted_values,
        resid = resid,
        GCV = GCV,
        R_Squared = R_Squared,
        Adj_R_Squared = Adj_R_Squared,
        scaled_x = X,
        x = x,
        with_kmeans = with_kmeans,
        cclust_obj = cclust_obj,
        scaled_x_clust = X_clust,
        response_y = response_y,
        fit_method = method
      )
      
      class(res) <- "matern32"
      
      return(res)
    }
    
    # 3 - 2 solve, chol, eigen -----
    if (method %in% c("solve", "chol", "eigen"))
    {
      if (length(reg_lambda) == 1)
      {
        if (!with_kmeans)
        {
          response_y_hat <- K %*% coef
        } else {
          K_star <- matern32_kxstar_cpp(newx = X,
                                        # X is already scaled
                                        x = X_clust,
                                        l = l)
          
          response_y_hat <- K_star %*% coef
        }
        
        # if (centering)
        # {
        #   fitted_values <- drop(ym +  response_y_hat)
        # } else {
          fitted_values <- drop(response_y_hat)
        #}
        
        resid <- response_y - response_y_hat
        
        RSS <- sum((y - fitted_values) ^ 2)
        TSS <- sum((y - ym) ^ 2)
        R_Squared <- 1 - RSS / TSS
        Adj_R_Squared <-
          1 - (1 - R_Squared) * ((n - 1) / (n - p - 1))
        
        res <- list(
          K = K,
          l = l,
          reg_lambda = reg_lambda,
          coef = drop(coef),
          #centering = centering,
          scales = x_scaled$xsd,
          ym = ym,
          xm = x_scaled$xm,
          fitted_values = fitted_values,
          resid = drop(resid),
          loocv = loocv,
          R_Squared = R_Squared,
          Adj_R_Squared = Adj_R_Squared,
          scaled_x = X,
          x = x,
          with_kmeans = with_kmeans,
          cclust_obj = cclust_obj,
          scaled_x_clust = X_clust,
          response_y = response_y,
          fit_method = method
        )
        
        class(res) <- "matern32"
        
        return(res)
      } else {
        if (!with_kmeans)
        {
          response_y_hat <- K %*% coefs # coef with s
        } else {
          K_star <- matern32_kxstar_cpp(newx = X,
                                        # X is already scaled
                                        x = X_clust,
                                        l = l)
          
          response_y_hat <- K_star %*% coefs # coef with s
        }
        
        # if (centering)
        # {
        #   fitted_values <- drop(ym +  response_y_hat)
        # } else {
          fitted_values <- drop(response_y_hat)
        #}
        
        resid <- response_y - response_y_hat
        
        RSS <- colSums((y - fitted_values) ^ 2)
        TSS <- sum((y - ym) ^ 2)
        R_Squared <- 1 - RSS / TSS
        names(R_Squared) <- reg_lambda
        Adj_R_Squared <-
          1 - (1 - R_Squared) * ((n - 1) / (n - p - 1))
        
        res <- list(
          K = K,
          l = l,
          reg_lambda = reg_lambda,
          coef = drop(coefs),
          #centering = centering,
          scales = x_scaled$xsd,
          ym = ym,
          xm = x_scaled$xm,
          fitted_values = fitted_values,
          resid = resid,
          loocv = loocv,
          R_Squared = R_Squared,
          Adj_R_Squared = Adj_R_Squared,
          scaled_x = X,
          x = x,
          with_kmeans = with_kmeans,
          cclust_obj = cclust_obj,
          scaled_x_clust = X_clust,
          response_y = response_y,
          fit_method = method
        )
        
        class(res) <- "matern32"
        
        return(res)
      }
    }
  }

fit_matern32_classification <-
  function(x,
           y,
           reg_lambda = 0.1,
           #10^seq(-5, 4, length.out = 100),
           l = NULL,
           method = c("chol", "solve", "svd", "eigen"),
           with_kmeans = FALSE,
           centers = NULL,
           #centering = FALSE,
           seed = 123,
           cl = NULL,
           ...)
  {
    stopifnot(length(reg_lambda) == 1L) # no multiple values of reg_lambda
    
    method <- match.arg(method)
    
    x <- as.matrix(x)
    stopifnot(is.factor(y))
    Y <- one_hot(y)
    n_classes <- dim(Y)[2]
    nreg_lambda <- length(reg_lambda)
    n <- dim(x)[1]
    p <- dim(x)[2]
    stopifnot(n == nrow(Y))
    one_reg_lambda <- (length(reg_lambda) <= 1)
    cclust_obj <- NULL
    
    # centered response?
    # if (centering)
    # {
    #   Ym <- colMeans(Y)
    #   response_Y <- my_scale(x = Y, xm = Ym)
    # } else {
      Ym <- colMeans(Y)
      response_Y <- Y
    #}
    
    # construct covariance
    x_scaled <- my_scale(x)
    X <- x_scaled$res
    X_clust <- NULL
    
    if (is.null(l))
      l <- sqrt(p)
    
    # compute kernel as a vector if necessary
    if (length(l) == 1)
      l <- rep(l, p)
    
    
    # 1 - compute kernel K -----
    
    # 1 - 1 - K for n > 500 -----
    if (n > 500)
      # can use kmeans
    {
      # 1 - 1 - 1 K with k-means -----
      if (with_kmeans == TRUE)
      {
        # adjust KRR to centers = X and this new response = y
        if (is.null(centers))
        {
          warning("with_kmeans == TRUE but 'centers' not provided: set to 100")
          centers <- 100
          
        }
        
        set.seed(seed)
        cclust_obj <-
          stats::kmeans(x = as.matrix(X), centers = centers)
        X_clust <- as.matrix(cclust_obj$centers)
        
        response_Y_clust <- parfor(args=1:centers,
                                   what=function(i)
                                     apply(response_Y[which(cclust_obj$cluster == i)], 2, median))
        K <- matern32_kxx_cpp(x = X_clust, l = l)
        
        
      } else {
        # 1 - 1 - 2 K without k-means -----
        cat(
          "Processing... (try using option 'with_kmeans' for faster results when nrow(x) > 500)",
          "\n"
        )
        K <- matern32_kxx_cpp(x = X, l = l)
        
      }
      
    } else {
      # 1 - 2 - K for n <= 500 -----
      if (with_kmeans == TRUE)
      {
        # 1 - 2 - 1 with k-means -----
        stop("option 'with_kmeans' not useful for n_obs <= 500")
      } else {
        # 1 - 2 - 2 without k-means -----
        K <- matern32_kxx_cpp(x = X, l = l)
      }
      
    }
    
    
    # 2 - compute coeffs -----
    
    if (with_kmeans == FALSE)
    {
      # 2 - 1 - without k-means -----
      
      if (method == "solve")
      {
        if (one_reg_lambda)
        {
          K_plus <- K + reg_lambda * diag(dim(K)[1])
          invK <- solve(K_plus)
          coef <- invK %*% response_Y
          cat("coef", "\n")
          print(coef)
          cat("\n")
          loocv <- sum(drop(coef / diag(invK)) ^ 2)
        } else {
          # length(reg_lambda) > 1
          
          get_loocv <- function(reg_lambda_i)
          {
            K_plus <- K + reg_lambda_i * diag(dim(K)[1])
            invK <- solve(K_plus)
            coef <- invK %*% response_Y
            cat("coef", "\n")
            print(coef)
            cat("\n")
            return(list(coef = coef,
                        loocv = drop(coef / diag(invK))))
          }
          
          fit_res <- lapply(reg_lambda, function(x)
            get_loocv(x))
          n_fit_res <- length(fit_res)
          
          loocv <- colSums(parfor(args=1:n_fit_res,
                                  what=function(i)
                                    fit_res[[i]]$loocv) ^ 2)
          names(loocv) <- reg_lambda
          
          coefs <- parfor(args=1:n_fit_res, what=function(i)
            fit_res[[i]]$coef)
          colnames(coefs) <- reg_lambda
        }
      }
      
      if (method == "svd")
      {
        Xs <- La.svd(K)
        rhs <- crossprod(Xs$u, response_Y)
        d <- Xs$d
        nb_di <- length(d)
        div <- d ^ 2 + rep(reg_lambda, rep(nb_di, nreg_lambda))
        a <- drop(d * rhs) / div
        dim(a) <- c(nb_di, nreg_lambda)
        coef <- crossprod(Xs$vt, a)
        cat("coef", "\n")
        print(coef)
        cat("\n")
        # colnames(coef) <- reg_lambda # /!\ can't work
        
        response_Y_hat <- K %*% coef
        
        # if (centering)
        # {
        #   fitted_values <- drop(Ym +  response_Y_hat)
        # } else {
          fitted_values <- drop(response_Y_hat)
        #}
        
        resid <- response_Y - response_Y_hat
        # colnames(resid) <- reg_lambda # /!\ can't work
        # GCV <- colSums(resid^2)/(nrow(X) - colSums(matrix(d^2/div,
        #                                                  nb_di)))^2 # /!\ can't work
        
        
        RSS <- colSums((Y - fitted_values) ^ 2)
        TSS <- colSums((Y - Ym) ^ 2)
        R_Squared <- 1 - RSS / TSS
        # names(R_Squared) <- reg_lambda # /!\ can't work
        Adj_R_Squared <-
          1 - (1 - R_Squared) * ((n - 1) / (n - p - 1))
        
        res <- list(
          K = K,
          l = l,
          reg_lambda = reg_lambda,
          coef = drop(coef),
          #centering = centering,
          scales = x_scaled$xsd,
          ym = Ym,
          xm = x_scaled$xm,
          fitted_values = fitted_values,
          resid = resid,
          GCV = GCV,
          R_Squared = R_Squared,
          Adj_R_Squared = Adj_R_Squared,
          scaled_x = X,
          with_kmeans = with_kmeans,
          cclust_obj = cclust_obj,
          scaled_x_clust = X_clust,
          x = x,
          response_Y = response_Y,
          fit_method = method
        )
        
        class(res) <- "matern32"
        
        return(res)
      }
      
      if (method == "chol")
      {
        if (one_reg_lambda)
        {
          K_plus <- K + reg_lambda * diag(dim(K)[1])
          invK <- chol2inv(chol(K_plus))
          coef <- invK %*% response_Y
          loocv <- sum(drop(coef / diag(invK)) ^ 2)
        } else {
          # length(reg_lambda) > 1
          
          get_loocv <- function(reg_lambda_i)
          {
            K_plus <- K + reg_lambda_i * diag(dim(K)[1])
            invK <- chol2inv(chol(K_plus))
            coef <- invK %*% response_Y
            return(list(coef = coef,
                        loocv = drop(coef / diag(invK))))
          }
          
          fit_res <- lapply(reg_lambda, function(x)
            get_loocv(x))
          n_fit_res <- length(fit_res)
          
          loocv <- colSums(parfor(args=1:n_fit_res,
                                  what=function(i)
                                    fit_res[[i]]$loocv) ^ 2)
          names(loocv) <- reg_lambda
          
          coefs <- parfor(args=1:n_fit_res, what=function(i)
            fit_res[[i]]$coef)
          colnames(coefs) <- reg_lambda
        }
      }
      
      if (method == "eigen")
      {
        if (one_reg_lambda)
        {
          eigenK <- base::eigen(K)
          eigen_values <- eigenK$values
          Q <- eigenK$vectors
          inv_eigen <- solve_eigen(
            Eigenvectors = Q,
            Eigenvalues = eigen_values,
            y = response_Y,
            reg_lambda = reg_lambda
          )
          
          coef <- inv_eigen$coeffs
          loocv <- sum(inv_eigen$loocv ^ 2)
        } else {
          # length(reg_lambda) > 1
          
          get_loocv <- function(reg_lambda_i)
          {
            eigenK <- base::eigen(K)
            eigen_values <- eigenK$values
            Q <- eigenK$vectors
            inv_eigen <- solve_eigen(
              Eigenvectors = Q,
              Eigenvalues = eigen_values,
              y = response_Y,
              reg_lambda = reg_lambda_i
            )
            return(list(coef = inv_eigen$coef,
                        loocv = inv_eigen$loocv))
          }
          
          fit_res <- lapply(reg_lambda, function(x)
            get_loocv(x))
          n_fit_res <- length(fit_res)
          
          loocv <- colSums(parfor(args=1:n_fit_res,
                                  what=function(i)
                                    fit_res[[i]]$loocv) ^ 2)
          names(loocv) <- reg_lambda
          
          coefs <- parfor(args=1:n_fit_res, what=function(i)
            fit_res[[i]]$coef)
          colnames(coefs) <- reg_lambda
          
        }
      }
      
    } else {
      # 2 - 2 - with k-means -----
      if (n > 500)
      {
        # 2 - 2 - 1 For n > 500 -----
        
        if (method %in% c("chol", "solve"))
        {
          if (one_reg_lambda)
          {
            K_plus <- K + reg_lambda * diag(dim(K)[1])
            invK <- switch(method,
                           "solve" = solve(K_plus),
                           "chol" = chol2inv(chol(K_plus)))
            coef <- invK %*% response_Y_clust
            loocv <- sum(drop(coef / diag(invK)) ^ 2)
          } else {
            # length(reg_lambda) > 1
            
            get_loocv <- function(reg_lambda_i)
            {
              K_plus <- K + reg_lambda_i * diag(dim(K)[1])
              invK <- switch(method,
                             "solve" = solve(K_plus),
                             "chol" = chol2inv(chol(K_plus)))
              coef <- invK %*% response_Y_clust
              return(list(coef = coef,
                          loocv = drop(coef / diag(invK))))
            }
            
            fit_res <- lapply(reg_lambda, function(x)
              get_loocv(x))
            n_fit_res <- length(fit_res)
            
            loocv <- colSums(parfor(args=1:n_fit_res,
                                    what=function(i)
                                      fit_res[[i]]$loocv) ^ 2)
            names(loocv) <- reg_lambda
            
            coefs <-
              parfor(args=1:n_fit_res, what=function(i)
                fit_res[[i]]$coef)
            # colnames(coefs) <- reg_lambda # /!\ can't work
          }
        }
        
        if (method == "svd")
        {
          Xs <- La.svd(K) # K is based on clustered X
          rhs <- crossprod(Xs$u, response_Y_clust)
          d <- Xs$d
          nb_di <- length(d)
          div <- d ^ 2 + rep(reg_lambda, rep(nb_di, nreg_lambda))
          a <- drop(d * rhs) / div
          dim(a) <- c(nb_di, nreg_lambda)
          coef <- crossprod(Xs$vt, a)
          colnames(coef) <- reg_lambda
          scales <- x_scaled$xsd
          xm <- x_scaled$xm
          
          K_star <-
            matern32_kxstar_cpp(newx = X,
                                # X is already scaled
                                x = X_clust,
                                l = l)
          
          response_Y_hat <- K_star %*% coef
          
          # if (centering)
          # {
          #   fitted_values <- drop(Ym +  response_Y_hat)
          # } else {
            fitted_values <- drop(response_Y_hat)
          #}
          
          resid <- response_Y - response_Y_hat
          colnames(resid) <- reg_lambda
          GCV <-
            colSums(resid ^ 2) / (nrow(X) - colSums(matrix(d ^ 2 / div,
                                                           nb_di))) ^ 2
          
          if (length(reg_lambda) > 1)
          {
            RSS <- colSums((Y - fitted_values) ^ 2)
          } else {
            RSS <- sum((Y - fitted_values) ^ 2)
          }
          
          TSS <- colSums((Y - Ym) ^ 2)
          R_Squared <- 1 - RSS / TSS
          Adj_R_Squared <-
            1 - (1 - R_Squared) * ((n - 1) / (n - p - 1))
          # names(R_Squared) <- reg_lambda # /!\ can't work
          
        }
        
      } else {
        # 2 - 2 - 2 For n <= 500 -----
        stop("option 'with_kmeans' not implemented for n_obs <= 500")
        
      }
      
    }
    
    
    # 3 - returns -----
    
    
    # 3 - 1 svd -----
    if (method == "svd")
    {
      res <- list(
        K = K,
        l = l,
        reg_lambda = reg_lambda,
        coef = drop(coef),
        #centering = centering,
        scales = scales,
        ym = Ym,
        xm = xm,
        fitted_values = fitted_values,
        resid = resid,
        GCV = GCV,
        R_Squared = R_Squared,
        Adj_R_Squared = Adj_R_Squared,
        scaled_x = X,
        x = x,
        with_kmeans = with_kmeans,
        cclust_obj = cclust_obj,
        scaled_x_clust = X_clust,
        response_Y = response_Y,
        fit_method = method
      )
      
      class(res) <- "matern32"
      
      return(res)
    }
    
    # 3 - 2 solve, chol, eigen -----
    if (method %in% c("solve", "chol", "eigen"))
    {
      if (length(reg_lambda) == 1)
      {
        if (!with_kmeans)
        {
          response_Y_hat <- K %*% coef
        } else {
          K_star <- matern32_kxstar_cpp(newx = X,
                                        # X is already scaled
                                        x = X_clust,
                                        l = l)
          
          response_Y_hat <- K_star %*% coef
        }
        
        # if (centering)
        # {
        #   fitted_values <- drop(Ym +  response_Y_hat)
        # } else {
          fitted_values <- drop(response_Y_hat)
        #}
        
        resid <- response_Y - response_Y_hat
        
        RSS <- colSums((Y - fitted_values) ^ 2)
        TSS <- colSums((Y - Ym) ^ 2)
        R_Squared <- 1 - RSS / TSS
        Adj_R_Squared <-
          1 - (1 - R_Squared) * ((n - 1) / (n - p - 1))
        
        res <- list(
          K = K,
          l = l,
          reg_lambda = reg_lambda,
          coef = drop(coef),
          #centering = centering,
          scales = x_scaled$xsd,
          ym = Ym,
          xm = x_scaled$xm,
          fitted_values = fitted_values,
          resid = drop(resid),
          loocv = loocv,
          R_Squared = R_Squared,
          Adj_R_Squared = Adj_R_Squared,
          scaled_x = X,
          x = x,
          with_kmeans = with_kmeans,
          cclust_obj = cclust_obj,
          scaled_x_clust = X_clust,
          response_Y = response_Y,
          fit_method = method
        )
        
        class(res) <- "matern32"
        
        return(res)
      } else {
        if (!with_kmeans)
        {
          response_Y_hat <- K %*% coefs # coef with s
        } else {
          K_star <- matern32_kxstar_cpp(newx = X,
                                        # X is already scaled
                                        x = X_clust,
                                        l = l)
          
          response_Y_hat <- K_star %*% coefs # coef with s
        }
        
        # if (centering)
        # {
        #   fitted_values <- drop(Ym +  response_Y_hat)
        # } else {
          fitted_values <- drop(response_Y_hat)
        #}
        
        resid <- response_Y - response_Y_hat
        
        RSS <- colSums((Y - fitted_values) ^ 2)
        TSS <- colSums((Y - Ym) ^ 2)
        R_Squared <- 1 - RSS / TSS
        # names(R_Squared) <- reg_lambda # /!\ can't work
        Adj_R_Squared <-
          1 - (1 - R_Squared) * ((n - 1) / (n - p - 1))
        
        res <- list(
          K = K,
          l = l,
          reg_lambda = reg_lambda,
          coef = drop(coefs),
          #centering = centering,
          scales = x_scaled$xsd,
          ym = Ym,
          xm = x_scaled$xm,
          fitted_values = fitted_values,
          resid = resid,
          loocv = loocv,
          R_Squared = R_Squared,
          Adj_R_Squared = Adj_R_Squared,
          scaled_x = X,
          x = x,
          with_kmeans = with_kmeans,
          cclust_obj = cclust_obj,
          scaled_x_clust = X_clust,
          response_Y = response_Y,
          fit_method = method
        )
        
        class(res) <- "matern32"
        
        return(res)
      }
    }
  }

# 3 - 2 predict matern32 -------------------------------------------------------------------

predict_matern32 <- function(fit_obj, newx, ci = NULL)
{
  if (is.vector(newx))
    newx <- t(newx)
  
  # if (fit_obj$centering)
  # {
  #   # response was centered
  #   if (!is.null(fit_obj$with_kmeans))
  #   {
  #     K_star <- matern32_kxstar_cpp(
  #       newx = as.matrix(my_scale(
  #         x = newx,
  #         xm = as.matrix(fit_obj$xm),
  #         xsd = as.vector(fit_obj$scales)
  #       )),
  #       x = fit_obj$scaled_x_clust,
  #       l = fit_obj$l
  #     )
  #     
  #     #print("here1 -----")
  #     try_return <-
  #       try(return(drop(crossprod(K_star %*% fit_obj$coef)) + fit_obj$ym),
  #           silent = TRUE)
  #     if (inherits(try_return, "try-error"))
  #     {
  #       #print("here2 -----")
  #       return(drop(crossprod(K_star %*% fit_obj$coef)) + fit_obj$Ym)
  #     }
  #   } else {
  #     K_star <- matern32_kxstar_cpp(
  #       newx = as.matrix(my_scale(
  #         x = newx,
  #         xm = as.vector(fit_obj$xm),
  #         xsd = as.vector(fit_obj$scales)
  #       )),
  #       x = as.matrix(fit_obj$scaled_x),
  #       l = as.vector(fit_obj$l)
  #     )
  #     
  #     #print("here3 -----")
  #     try_return <-
  #       try(return(drop(K_star %*% fit_obj$coef) + fit_obj$ym),
  #           silent = TRUE)
  #     if (inherits(try_return, "try-error"))
  #     {
  #       #print("here4 -----")
  #       return(drop(crossprod(K_star %*% fit_obj$coef)) + fit_obj$Ym)
  #     }
  #   }
  #   
  # } else {
    # response wasn't centered
    if (fit_obj$with_kmeans)
    {
      K_star <- matern32_kxstar_cpp(
        newx = as.matrix(my_scale(
          x = newx,
          xm = as.vector(fit_obj$xm),
          xsd = as.vector(fit_obj$scales)
        )),
        x = fit_obj$scaled_x_clust,
        l = fit_obj$l
      )
      
      #print("here5 -----")
      return(drop(crossprod(K_star %*% fit_obj$coef)))
    } else {
      K_star <- matern32_kxstar_cpp(
        newx = as.matrix(my_scale(
          x = newx,
          xm = as.vector(fit_obj$xm),
          xsd = as.vector(fit_obj$scales)
        )),
        x = fit_obj$scaled_x,
        l = fit_obj$l
      )
      #print("here6 -----")
      return(drop(K_star %*% fit_obj$coef))
    }
    
  #}
  
}
