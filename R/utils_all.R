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
    stopifnot(n == length(preds_set_list))
    for (i in seq_len(n))
    {
      res[i] <- (y_test[i] %in% preds_set_list[[i]])+0
    } 
  } else { # is.matrix 
    stopifnot(n == nrow(preds_set_list))
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
              "sigmoid" = function(x) 1/(1 + exp(-x)),
              "tanh" = function(x) tanh(x),
              "leakyrelu" = function(x) x*(x > 0) + 0.01*x*(x <= 0),
              "elu" = function(x) x*(x >= 0) + 0.01*(exp(x)-1)*(x < 0),
              "linear" = function(x) x)
  
  p <- ncol(x)
  set.seed(1)
  w <- remove_zero_cols(switch(match.arg(nodes_sim),
                               "sobol" = 2*t(randtoolbox::sobol(nb_hidden + 1, p)) - 1,
                               "halton" = 2*t(randtoolbox::halton(nb_hidden, p)) - 1,
                               "unif" = matrix(runif(nb_hidden*p, min = -1, max = 1),
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
  temp <- 1 / (1 + exp(-x))
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
    return(pkg_name %in% rownames(utils::installed.packages()))
  return(FALSE)
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
if (is_package_available("skimr") == FALSE)
{
  utils::install.packages("skimr")
}
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
    return(sample(x, n, replace = TRUE) + rnorm(n, sd=width))    # Here's the entire algorithm
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
  X_sd <- apply(X, 2, sd)
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

winkler_score <- function(obj, actual, level = 95) {
  alpha <- 1 - level / 100
  lt <- obj$lower
  ut <- obj$upper
  n_points <- length(actual)
  stopifnot((n_points == length(lt)) && (n_points == length(ut)))
  diff_lt <- lt - actual
  diff_bounds <- ut - lt
  diff_ut <- actual - ut
  score <-
    diff_bounds + (2 / alpha) * (pmax(diff_lt, 0) + pmax(diff_ut, 0))
  return(mean(score))
}

# Stratify stuff ----- from https://gist.github.com/mrdwab/6424112 stratified
# <- function(df, group, size, select = NULL, replace = FALSE, bothSets =
# FALSE) { if (is.null(select)) { df <- df } else { if (is.null(names(select)))
# stop(''select' must be a named list') if (!all(names(select) %in% names(df)))
# stop('Please verify your 'select' argument') temp <- sapply(names(select),
# function(x) df[[x]] %in% select[[x]]) df <- df[rowSums(temp) ==
# length(select), ] } df.interaction <- interaction(df[group], drop = TRUE)
# df.table <- table(df.interaction) df.split <- split(df, df.interaction) if
# (length(size) > 1) { if (length(size) != length(df.split)) stop('Number of
# groups is ', length(df.split), ' but number of sizes supplied is ',
# length(size)) if (is.null(names(size))) { n <- setNames(size,
# names(df.split)) message(sQuote('size'), ' vector entered as:\n\nsize =
# structure(c(', paste(n, collapse = ', '), '),\n.Names = c(',
# paste(shQuote(names(n)), collapse = ', '), ')) \n\n') } else {
# ifelse(all(names(size) %in% names(df.split)), n <- size[names(df.split)],
# stop('Named vector supplied with names ', paste(names(size), collapse = ',
# '), '\n but the names for the group levels are ', paste(names(df.split),
# collapse = ', '))) } } else if (size < 1) { n <- round(df.table * size,
# digits = 0) } else if (size >= 1) { if (all(df.table >= size) ||
# isTRUE(replace)) { n <- setNames(rep(size, length.out = length(df.split)),
# names(df.split)) } else { # message( # 'Some groups\n---', #
# paste(names(df.table[df.table < size]), collapse = ', '), # '---\ncontain
# fewer observations', # ' than desired number of samples.\n', # 'All
# observations have been returned from those groups.') n <-
# c(sapply(df.table[df.table >= size], function(x) x = size), df.table[df.table
# < size]) } } temp <- lapply( names(df.split), function(x)
# df.split[[x]][sample(df.table[x], n[x], replace = replace), ]) set1 <-
# do.call('rbind', temp) if (isTRUE(bothSets)) { set2 <- df[!rownames(df) %in%
# rownames(set1), ] list(SET1 = set1, SET2 = set2) } else { set1 } }
