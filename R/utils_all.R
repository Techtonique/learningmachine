# Useful functions for learningmachine (alphabetical order)

# compute scores for conformal "score" method -----
compute_scores <- function(probs, y_calibration_sc) {
  probs_calibration_sc <- probs*y_calibration_sc
  true_idx <- sapply(1:nrow(probs), 
                     function(i) which(probs_calibration_sc[i, ] > 0))
  f <- sapply(seq_len(nrow(probs)), function(i) (probs[i, true_idx[i]]))
  return(1 - f)
}

coverage_rate_classifier <- function(y_test, preds_set_list) {
  n <- length(y_test)
  res <- rep(0, n)
  if (is.list(preds_set_list))
  {
    #stopifnot(n == length(preds_set_list))
    for (i in seq_len(n))
    {
      res[i] <- (y_test[i] %in% preds_set_list[[i]])+0
    } 
  } else { # is.matrix 
    #stopifnot(n == nrow(preds_set_list))
    for (i in seq_len(n))
    {
      res[i] <- (y_test[i] == preds_set_list[i])+0
    }
  }
  return(mean(res)*100)
}

# create new predictors ---------------------------------------------------

remove_zero_cols <- function(x)
{
  x[, colSums(x == 0) != nrow(x)]
}

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# create new predictors
create_new_predictors <- function(x, nb_hidden = 5,
                                  nodes_sim = c("sobol", "halton", "unif"),
                                  activ = c("relu", "sigmoid", "tanh",
                                            "leakyrelu", "elu", "linear"),
                                  nn_xm = NULL, nn_scales = NULL)
{
  x <- as.matrix(x)
  if (identical(nb_hidden, 0))
  {
    return(x)
  }
  g <- switch(match.arg(activ),
              "relu" = function(x) x*(x>0),
              "sigmoid" = function(x) (1/(1 + exp(-x)))*(x >= 0) + (exp(x)/(1 + exp(x)))*(x < 0),
              "tanh" = function(x) tanh(x),
              "leakyrelu" = function(x) x*(x > 0) + 0.01*x*(x <= 0),
              "elu" = function(x) x*(x >= 0) + 0.01*(exp(x)-1)*(x < 0),
              "linear" = function(x) x)
  
  p <- ncol(x)
  set.seed(1)
  w <- remove_zero_cols(switch(match.arg(nodes_sim),
                               "sobol" = 2*t(randtoolbox::sobol(nb_hidden + 1, p)) - 1,
                               "halton" = 2*t(randtoolbox::halton(nb_hidden, p)) - 1,
                               "unif" = matrix(stats::runif(nb_hidden*p, min = -1, max = 1),
                                               nrow = p, ncol = nb_hidden)))
  
  if((!is.null(nn_xm) && is.null(nn_scales)) || (is.null(nn_xm) && !is.null(nn_scales)))
    stop("either nn_xm and nn_scales provided, or both left to NULL")
  
  if (is.null(nn_xm) && is.null(nn_scales))
  {
    scaled_x <- my_scale(x)
    hidden <- g(scaled_x$res%*%w)
    res <- cbind(x, hidden)
    
    if (length(colnames(x)) > 0){
      colnames(res) <- c(colnames(x),
                         paste0("h", 1:ncol(hidden)))
    } else {
      colnames(res) <- c(paste0("x", 1:ncol(x)),
                         paste0("h", 1:ncol(hidden)))
    }
    
    return(list(nn_xm = scaled_x$xm,
                nn_scales = scaled_x$xsd,
                w = w, predictors = res))
  }
  
  if (!is.null(nn_xm) && !is.null(nn_scales))
  {
    stopifnot(length(nn_xm) == ncol(x) || length(nn_scales) == ncol(x))
    scaled_x <- my_scale(as.matrix(x),
                         xm = as.vector(nn_xm),
                         xsd = as.vector(nn_scales))
    hidden <- g(as.matrix(scaled_x)%*%w)
    res <- cbind(x, hidden)
    
    if (length(colnames(x)) > 0){
      colnames(res) <- c(colnames(x),
                         paste0("h", 1:ncol(hidden)))
    } else {
      colnames(res) <- c(paste0("x", 1:ncol(x)),
                         paste0("h", 1:ncol(hidden)))
    }
    
    return(list(w = w, predictors = res))
  }
}

# prehistoric stuff -----
#' @export
debug_print <- function(x) {
  cat("\n")
  print(paste0(deparse(substitute(x)), "'s value:"))
  print(x)
  cat("\n")
}

# decode factors -----
decode_factors <- function(numeric_factor, encoded_factors) {
  encoded_factors <- encoded_factors$encoded_factors
  n_levels <- length(encoded_factors)
  res <-
    sapply(
      numeric_factor,
      FUN = function(i)
        encoded_factors[[i]]
    )
  factor(res, 
         levels = as.character(unlist(encoded_factors)))
}

decode_factors2 <- function(numeric_factor_list) {
  res <- sapply(numeric_factor_list, function(elt) {
    ans <- names(elt)
    if(is.null(ans))
    {
      return(elt)
    } else {
      return(ans)
    }
  })
  return(res)
}

# Correspondance factors -----
encode_factors <- function(y) {
  stopifnot(is.factor(y))
  levels_y <- levels(unique(y))
  n_levels <- length(levels_y)
  numeric_levels <- unique(as.integer(y))
  res <- lapply(1:n_levels, function(i)
    levels_y[i])
  names(res) <- numeric_levels
  return(list(numeric_factor = as.numeric(y), encoded_factors = res))
}

# Expit function -----
expit <- function(x) {
  (1 / (1 + exp(-x)))*(x >= 0) + (exp(x)/(1 + exp(x)))*(x < 0)
}
expit <- compiler::cmpfun(expit)

# get classes index -----
get_classes_idx <- function(new_probs, q_threshold, level) {
  if (is.matrix(new_probs))
  {
    n_obs <- nrow(new_probs) 
  } else {
    if (!is.null(new_probs$sims))
    {
      n_obs <- nrow(new_probs$sims[[1]])
    } else {
      stop("check dimensions of input probs") 
    }
  }
  
  res <- vector("list", n_obs)
  
  if (is.list(new_probs))
  {
    new_probs_ <- expit_probs(sapply(new_probs$sims, function(x) rowMeans(x)))
    if (!is.null(names(new_probs$sims)))
    {
      names(new_probs_) <- names(new_probs$sims)
    }
  } else {
    new_probs_ <- expit_probs(new_probs)
  }
  
  for (i in seq_len(n_obs))
  {
    ans <- which(new_probs_[i, ] >= (1 - q_threshold))
    if(length(names(ans)))
    { 
      res[[i]] <- ans 
    } 
  }
  return(res)
}


# get expit probs -----
expit_probs <- function(x) {
  temp <- (1 / (1 + exp(-x)))*(x >= 0)
  temp <- temp + (exp(x)/(1 + exp(x)))*(x < 0)  
  return(sweep(x=temp, MARGIN=1, FUN="/", 
               STATS=rowSums(temp)))
}


# get jackknife residuals -----
get_jackknife_residuals <-
  function(X, y, idx, fit_func, predict_func) {
    stopifnot(length(idx) == (nrow(X) - 1))
    X_train <- X[idx, ]
    y_train <- y[idx]
    X_test <- X[-idx, ]
    y_test <- y[-idx]
    #print()
    #stopifnot(is.null(dim(X_test)))
    fit_obj <- fit_func(X_train, y_train)
    preds <- predict_func(fit_obj, rbind(X_test, X_test))
    return(list(
      abs_residuals = abs(preds[1] - y_test),
      raw_residuals = preds[1] - y_test
    ))
  }

# Function to get key by value
get_key_by_value <- function(list_, value) {
  keys <- names(list_)
  values <- unlist(list_)
  if ("0" %in% keys)
  {
    res <- as.numeric(keys[which(values == value)][1])
    return(res + 1)
  }
  return(as.numeric(keys[which(values == value)][1]))
}

# get threshold for score conformal classification -----
get_threshold <- function(probs, y_calibration_sc, level) {
  scores <- compute_scores(probs, y_calibration_sc)
  return(quantile_scores(scores, 
                         level))
}

# impute classes when conformal classification finds NULL -----
impute_classes <- function(list_classes, probs)
{
  n_obs <- length(list_classes)
  for (i in seq_len(n_obs))
  {
    if (is.null(list_classes[[i]]))
    {
      list_classes[[i]] <- names(which.max(probs[i, ]))
    }
  }
  return(list_classes)
}

# Check if package is available -----
is_package_available <- function(pkg_name) {
  if (!is.null(pkg_name))
  {
    return(pkg_name %in% rownames(utils::installed.packages()))
  } else {
    return(TRUE) 
  }
}
is_package_available <- memoise::memoise(is_package_available)

# Check is whole number ------
is_wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}

# scaling matrices -----
my_scale <- function(x, xm = NULL, xsd = NULL) {
  rep_1_n <- rep.int(1, dim(x)[1])
  
  # centering and scaling, returning the means and sd's
  if (is.null(xm) && is.null(xsd)) {
    xm <- colMeans(x)
    xsd <- my_sd(x)
    return(list(
      res = (x - tcrossprod(rep_1_n, xm)) / tcrossprod(rep_1_n, xsd),
      xm = xm,
      xsd = xsd
    ))
  }
  
  # centering and scaling
  if (is.numeric(xm) && is.numeric(xsd)) {
    return((x - tcrossprod(rep_1_n, xm)) / tcrossprod(rep_1_n, xsd))
  }
  
  # centering only
  if (is.numeric(xm) && is.null(xsd)) {
    return(x - tcrossprod(rep_1_n, xm))
  }
  
  # scaling only
  if (is.null(xm) && is.numeric(xsd)) {
    return(x / tcrossprod(rep_1_n, xsd))
  }
}
my_scale <- compiler::cmpfun(my_scale)

# prettify summary -----
my_skim <- skimr::skim_with(numeric = skimr::sfl(),
                            base = skimr::sfl(),
                            append = TRUE)

# calculate std's of columns -----
my_sd <- function(x) {
  n <- dim(x)[1]
  return(drop(rep(1 / (n - 1), n) %*% (x - tcrossprod(
    rep.int(1, n), colMeans(x)
  )) ^ 2) ^ 0.5)
}
my_sd <- compiler::cmpfun(my_sd)

# one-hot encoding -----
one_hot <- function(y) {
  class_names <- as.character(levels(y))
  y <- as.numeric(y)
  n_obs <- length(y)
  n_classes <- length(unique(y))
  res <- matrix(0, nrow = n_obs, ncol = n_classes)
  colnames(res) <- class_names
  
  for (i in 1:n_obs) {
    res[i, y[i]] = 1
  }
  
  return(res)
}

# parallel for loop -----
parfor <- function(what,
                   args,
                   cl = NULL,
                   combine = c,
                   errorhandling = c("stop",
                                     "remove",
                                     "pass"),
                   verbose = FALSE,
                   show_progress = TRUE,
                   export = NULL,
                   ...)
{
  errorhandling <- match.arg(errorhandling)
  what <- compiler::cmpfun(what)
  
  n_iter <- length(args)
  
  if (!is.null(cl)) {
    # parallel
    stopifnot(is_wholenumber(cl) && cl > -2)
    if (cl == -1)
    {
      cl_SOCK <-
        parallel::makeCluster(parallel::detectCores(), type = "SOCK")
    } else {
      cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    }
    doSNOW::registerDoSNOW(cl_SOCK)
    `%op%` <- foreach::`%dopar%`
  } else {
    # sequential
    `%op%` <- foreach::`%do%`
  }
  
  if (show_progress)
  {
    pb <- utils::txtProgressBar(min = 0,
                                max = n_iter,
                                style = 3)
    progress <- function(n) {
      utils::setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)
  } else {
    opts <- NULL
  }
  
  i <- NULL
  res <- foreach::foreach(
    i = 1:n_iter,
    .combine = combine,
    .errorhandling = errorhandling,
    .options.snow = opts,
    .verbose = verbose,
    .export = export
  ) %op% {
    if (identical(show_progress, TRUE))
    {
      utils::setTxtProgressBar(pb, i)
    }
    
    res <- do.call(what = what,
                   args = c(list(args[i]), ...))
    
    as.numeric(res)
  }
  
  if (show_progress)
  {
    close(pb)
  }
  
  if (!is.null(cl))
  {
    snow::stopCluster(cl_SOCK)
  }
  
  return(unlist(res))
}

# permutation test -----
permutation_test <- function(x, y,
                             level = 95,
                             seed = 123) {
  set.seed(seed)
  stopifnot(length(x) == length(y))
  n <- length(x)
  xy <- x - y
  set.seed(seed)
  perms <- sample(xy, n)
}

# Quantile split conformal prediction -----

# classification
quantile_scores <- function(x, level) {
  alpha <- 1 - level/100
  n <- length(x)
  q_level <- ceiling((n + 1)*(1 - alpha))/n
  return(stats::quantile(x, probs=q_level))
}

# regression
quantile_scp <- function(abs_residuals, alpha) {
  n_cal_points <- length(abs_residuals)
  k <- ceiling((0.5 * n_cal_points + 1) * (1 - alpha))
  return(rank(abs_residuals)[k])
}

# Simulate using bootstrap -----
rbootstrap <- function(x,
                       n = length(x),
                       p = 1,
                       seed = 123) {
  if (p <= 1)
  {
    set.seed(seed)
    return(sample(x, size = n, replace = TRUE))
  } else {
    return(sapply(1:p,
                  function(i) {
                    set.seed(seed + i - 1)
                    sample(x, size = n, replace = TRUE)
                  }))
  }
}

# Simulate Gaussian kernel density -----
rkernel <- function(n, width, seed) {
  set.seed(seed)
  #method <- match.arg(method)
  #if (!identical(method, "antithetic"))
  #{
  return(stats::rnorm(n, sd = width))
  # } else {
  #   half_n <- n %/% 2
  #   eps <- stats::rnorm(half_n, sd = width)
  #   if (2 * length(eps) < n)
  #   {
  #     return(c(eps,-eps, stats::rnorm(1, sd = width)))
  #   }
  #   return(sample(c(eps,-eps),
  #                 replace = FALSE))
  # }
}  # Kernel sampler

rgaussiandens <- function(x,
                          n = length(x),
                          p = 1,
                          seed = 123) {
  
  z <- try(stats::density(x, bw = "sj", 
                          kernel = "gaussian"),
           silent = TRUE)
  
  if (inherits(z, "try-error"))
    z <- stats::density(x, kernel = "gaussian")
  
  width <- z$bw # Kernel width
  
  set.seed(seed)
  if (p <= 1)
  {
    return(sample(x, n, replace = TRUE) + stats::rnorm(n, sd=width))    # Here's the entire algorithm
  } else {
    return(simulate_gaussian_mixture_cpp(x=x, 
                                         n=n, 
                                         p=p, 
                                         width = width))
  }
}

# Simulate using surrogate -----
rsurrogate <- function(x,
                       n = length(x),
                       p = 1,
                       seed = 123) {
  if (n > length(x))
  {
    stop("For surrogates, must have number of predictions < number of training observations")
  }
  if (p <= 1)
  {
    set.seed(seed)
    res <- tseries::surrogate(x, ns = p,
                              fft = TRUE)
    return(res[seq_len(n), ])
  } else {
    res <- sapply(1:p,
                  function(i) {
                    set.seed(seed + i - 1)
                    tseries::surrogate(x, ns = p,
                                       fft = TRUE)
                  })
    return(res[seq_len(n), ])
  }
}

remove_nulls <- function(x) {
  return(x[!is.null(x)])
}

scale_matrix <- function(X)
{
  X_mean <- colMeans(X)
  X_sd <- apply(X, 2, stats::sd)
  X <- sweep(X, 2, X_mean, "-")
  X <- sweep(X, 2, X_sd, "/")
  return(list(X = X, X_mean = X_mean, X_sd = X_sd))
}

# sort data frame -----
sort_df <- function(df, by, decreasing = FALSE) {
  return(df[order(df[[by]], decreasing = decreasing), ])
}

# Split a dataset -----
split_data <- function(y, p = 0.5, 
                       seed = 123,
                       type_split=c("stratify", 
                                    "sequential")) {
  
  type_split <- match.arg(type_split)
  
  if (identical(type_split, "sequential"))
  {
    return(seq_len(length(y)*p))
  }
  
  # from caret::createFolds
  set.seed(seed)
  stopifnot((p > 0) && (p < 1))
  k <- floor(1 / p)
  return_list <- TRUE
  returnTrain <- FALSE
  
  if (inherits(y, "Surv"))
    y <- y[, "time"]
  if (is.numeric(y)) {
    cuts <- floor(length(y) / k)
    if (cuts < 2)
      cuts <- 2
    if (cuts > 5)
      cuts <- 5
    breaks <-
      unique(stats::quantile(y, probs = seq(0, 1, length = cuts)))
    y <- cut(y, breaks, include.lowest = TRUE)
  }
  if (k < length(y)) {
    y <- factor(as.character(y))
    numInClass <- table(y)
    foldVector <- vector(mode = "integer", length(y))
    for (i in 1:length(numInClass)) {
      min_reps <- numInClass[i] %/% k
      if (min_reps > 0) {
        spares <- numInClass[i] %% k
        seqVector <- rep(1:k, min_reps)
        if (spares > 0)
          seqVector <- c(seqVector, sample(1:k, spares))
        foldVector[which(y == names(numInClass)[i])] <-
          sample(seqVector)
      } else {
        foldVector[which(y == names(numInClass)[i])] <-
          sample(1:k, size = numInClass[i])
      }
    }
  } else {
    foldVector <- seq(along = y)
  }
  
  out <- split(seq(along = y), foldVector)
  
  return(out[[1]])
}

#  winkler_score testing ----------
rescaled_winkler_score_test <- function(actual, lower, upper, level = 95) {
  diff_bounds <- upper - lower
  score <- diff_bounds # base score
  penalty_factor <- (2 / (1 - level / 100))
  score <- score + penalty_factor * pmax(lower - actual, 0) # penalty
  score <- score + penalty_factor * pmax(actual - upper, 0) # penalty
  stats <- 1 - score/diff_bounds
  res <- t.test(stats)
  return(list(
    pvalue = res$p.value,
    statistic = res$statistic,
    est = res$estimate,
    low = res$conf.int[1],
    up = res$conf.int[2]
  ))
}


# Define the function to split vector into three equal-sized parts
stratify_vector_equal_size <- function(vector, seed = 123) {
  # Set seed for reproducibility
  set.seed(seed)
  
  n <- length(vector)
  
  # First split: Create training and remaining (calibration + test)
  train_index <- drop(caret::createDataPartition(vector, p = 0.3, list = FALSE))
  train_data <- vector[train_index]
  remaining_data <- vector[-train_index]
  
  # Second split: Create calibration and test sets
  calib_index <- drop(caret::createDataPartition(remaining_data, p = 0.5, list = FALSE))
  calib_data <- remaining_data[calib_index]
  test_data <- remaining_data[-calib_index]
  
  # Return results as a list
  return(list(
    train = train_data,
    calib = calib_data,
    test = test_data
  ))
}

inverse_transform_kde <- function(data, n = 1000, ...) {
  #kde <- density(data, bw = "SJ", kernel = "epanechnikov")
  kde <- density(data, bw = "SJ", ...)
  prob <- kde$y / sum(kde$y)
  cdf <- cumsum(prob)
  
  # Ensure x-values and CDF values are unique
  unique_indices <- !duplicated(cdf)
  cdf_unique <- cdf[unique_indices]
  x_unique <- kde$x[unique_indices]
  
  # Generate uniform random numbers
  u <- runif(n)
  
  # Perform interpolation using unique CDF values
  simulated_data <- approx(cdf_unique, x_unique, u)$y
  
  # Replace NA values with the median of the interpolated values
  median_value <- median(simulated_data, na.rm = TRUE)
  simulated_data[is.na(simulated_data)] <- median_value
  
  return(simulated_data)
}

# Direct Sampling 
direct_sampling <- function(data = NULL, n = 1000, 
                            method = c("kde", 
                                       "surrogate", 
                                       "bootstrap"),
                            kde = NULL, 
                            seed = NULL,
                            ...) {  
  method <- match.arg(method)
  if (!is.null(seed))
  {
    set.seed(seed)   
  }    
  if (identical(method, "kde"))
  {
    if (is.null(kde)) {
      stopifnot(!is.null(data))
      kde <- density(data, bw = "SJ", ...)
    } else if (is.null(data))
    {
      stopifnot(!is.null(kde))
    }
    prob <- kde$y / sum(kde$y)    
    return(sample(kde$x, size = n, replace = TRUE, prob = prob))    
  }
  
  if (identical(method, "surrogate"))
  {
    return(sample(tseries::surrogate(data, ns = 1, ...), 
                  size = n, 
                  replace = TRUE))
  }                                    
  
  if (identical(method, "bootstrap"))
  {
    return(sample(tseries::tsbootstrap(data, nb = 1, 
    type = "block", b = 1, ...), 
                  size = n, 
                  replace = TRUE))
  }
}

t_test_from_ci <- function(mean_estimate, lower_bound, upper_bound, df, conf_level = 0.95, mu_0 = 0) {
  # Calculate the critical value of the t-distribution
  alpha <- 1 - conf_level
  t_alpha <- qt(1 - alpha / 2, df = Inf)  # df = Inf for large sample approximation (Z-distribution)
  
  # Compute the standard error (SE)
  SE <- (upper_bound - lower_bound) / (2 * t_alpha)
  
  # Compute the t-statistic
  t_stat <- (mean_estimate - mu_0) / SE
  
  # Calculate the p-value for the two-tailed test
  p_value <- 2 * pt(-abs(t_stat), df = df)
  
  # Return the p-value
  return(p_value)
}

# Non-parametric test using Wilcoxon Signed-Rank Test
wilcoxon_test <- function(x, mu_0 = 0) {
  # Perform a one-sample Wilcoxon signed-rank test
  test_result <- wilcox.test(x, mu = mu_0)
  return(test_result$p.value)
}

compute_ci_mean <- function(xx, 
                            type_split = c("random", 
                                           "sequential"), 
                            method = c("surrogate", 
                                       "bootstrap",
                                       "kde"),                            
                            level=95, 
                            kde=NULL,
                            seed = 123)
{
  B <- 500L
  type_split <- match.arg(type_split)
  method <- match.arg(method)  
  upper_prob <- 1 - 0.5*(1 - level/100)
  
  if (type_split == "random")
  {
    set.seed(seed)
    z <- stratify_vector_equal_size(xx, seed=seed)
    x_train <- z$train
    x_calib <- z$calib
    x_test <- z$test
    estimate_x <- base::mean(x_train)
    sd_x <- sd(x_train)
    
    calib_resids <- (x_calib - estimate_x)/sd_x # standardization => distro of gaps to the mean is homoscedastic and centered ('easier' to sample since stationary?)
    
    if (!is.null(kde))
    {
      sim_calib_resids <- base::replicate(n=B, direct_sampling(calib_resids, 
                                                            n=length(x_test), 
                                                            kde=kde, 
                                                            seed=NULL))
    } else {
      sim_calib_resids <- base::replicate(n=B, direct_sampling(calib_resids, 
                                                            n=length(x_test), 
                                                            method = method, 
                                                            seed=NULL))  
   } 
           
  pseudo_obs_x <- x_test + sd_x*sim_calib_resids
  pseudo_means_x <- colMeans(pseudo_obs_x)
  estimate <- stats::median(pseudo_means_x)
  lower <- quantile(pseudo_means_x, probs = 1 - upper_prob)
  upper <- quantile(pseudo_means_x, probs = upper_prob)
  pvalue <- wilcoxon_test(pseudo_means_x)

  return(list(
    estimate = estimate,
    lower = lower,
    upper = upper,
    pvalue = pvalue))

  }
}


bootstrap_ci_mean <- function(xx, level = 95, seed = 123) {

  set.seed(seed)
  B <- 1000L
  alpha <- 1 - level / 100

  bootstrap_means <- rep(0, B)  
  
  for (i in 1:B) {
    set.seed(seed + i)
    bootstrap_means[i] <- mean(sample(xx, replace = TRUE))
  }
  
  # Step 2: Calculate the confidence interval
  estimate <- median(bootstrap_means)
  lower_bound <- quantile(bootstrap_means, alpha / 2)
  upper_bound <- quantile(bootstrap_means, 1 - alpha / 2)  
  pvalue <- wilcoxon_test(bootstrap_means)

  return(list(
    estimate = estimate,
    lower = lower_bound,
    upper = upper_bound,    
    pvalue = pvalue))
}


boot_means_matrix <- function(B, mean_estimate, resids, seed) {
  set.seed(seed)
  n <- length(resids)
  # Add mean_estimate to each resampled residual and compute means
  colMeans(matrix(resids[matrix(sample(n, size = n * B, replace = TRUE), nrow = n, ncol = B)], 
  nrow = n, ncol = B) + mean_estimate)
}

conformal_ci_mean <- function(xx, level = 95, seed = 123, cpp=TRUE) {
  set.seed(seed)
  alpha <- 1 - (level / 100)
  n_xx <- length(xx)
  half_n_xx <- n_xx %/% 2
  idx_train <- sample(seq_len(n_xx), replace = FALSE, size = half_n_xx)

  # Calculate the mean estimate on the sampled training set
  mean_estimate <- mean(xx[idx_train])

  # Calculate residuals from the remaining data points
  resids <- xx[-idx_train] - mean_estimate

  # Bootstrap the residuals to estimate the variability
  B <- 500L  # Number of bootstrap samples

  if (cpp)
  {
    boot_samples <- matrix(fastSampleCpp(resids, B * length(resids)), ncol = B)
  } else {
    boot_samples <- matrix(sample(resids, size = B * length(resids), replace = TRUE), ncol = B)
  }
  boot_means <- colMeans(mean_estimate + boot_samples)

  # Derive the confidence interval from the bootstrap distribution of means
  lower <- as.numeric(quantile(boot_means, alpha / 2))
  upper <- as.numeric(quantile(boot_means, 1 - alpha / 2))
 
  return(list(
    estimate = median(boot_means),
    lower = lower,
    upper =  upper,
    pvalue = wilcoxon_test(boot_means)))
}