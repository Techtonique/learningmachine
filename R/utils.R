# Usefull functions for learningmachine (alphabetical order)

# Correspondance factors -----
encode_factors <- function(y) {
    stopifnot(is.factor(y))
    levels_y <- levels(unique(y))
    n_levels <- length(levels_y)
    numeric_levels <- unique(as.integer(y))
    res <- lapply(1:n_levels, function(i) levels_y[i])
    names(res) <- numeric_levels
    return(list(numeric_factor = as.numeric(y), encoded_factors = res))
}

decode_factors <- function(numeric_factor, encoded_factors) {
    encoded_factors <- encoded_factors$encoded_factors
    n_levels <- length(unique(numeric_factor))
    stopifnot(n_levels == length(encoded_factors))
    res <- sapply(numeric_factor, FUN = function(i) encoded_factors[[i]])
    factor(res, levels = as.character(unlist(encoded_factors)))
}

# Expit function -----
expit <- function(x) {
    1/(1 + exp(-x))
}

# get jackknife residuals -----
get_jackknife_residuals <- function(X, y, idx, fit_func, predict_func) {
    stopifnot(length(idx) == (nrow(X) - 1))
    X_train <- X[idx, ]
    y_train <- y[idx]
    X_test <- X[-idx, ]
    y_test <- y[-idx]
    #print()
    #stopifnot(is.null(dim(X_test)))
    fit_obj <- fit_func(X_train, y_train)
    preds <- predict_func(fit_obj, rbind(X_test, X_test))
    return(list(abs_residuals = abs(preds[1] - y_test), 
                raw_residuals = preds[1] - y_test))
}


# Check if package is available -----
is_package_available <- function(pkg_name) {
    return(pkg_name %in% rownames(utils::installed.packages()))
}

# Check is whole number ------
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
}

# scaling matrices -----
my_scale <- function(x, xm = NULL, xsd = NULL) {
    rep_1_n <- rep.int(1, dim(x)[1])

    # centering and scaling, returning the means and sd's
    if (is.null(xm) && is.null(xsd)) {
        xm <- colMeans(x)
        xsd <- my_sd(x)
        return(list(res = (x - tcrossprod(rep_1_n, xm))/tcrossprod(rep_1_n, xsd),
            xm = xm, xsd = xsd))
    }

    # centering and scaling
    if (is.numeric(xm) && is.numeric(xsd)) {
        return((x - tcrossprod(rep_1_n, xm))/tcrossprod(rep_1_n, xsd))
    }

    # centering only
    if (is.numeric(xm) && is.null(xsd)) {
        return(x - tcrossprod(rep_1_n, xm))
    }

    # scaling only
    if (is.null(xm) && is.numeric(xsd)) {
        return(x/tcrossprod(rep_1_n, xsd))
    }
}
my_scale <- compiler::cmpfun(my_scale)

# calculate std's of columns -----
my_sd <- function(x) {
    n <- dim(x)[1]
    return(drop(rep(1/(n - 1), n) %*% (x - tcrossprod(rep.int(1, n), colMeans(x)))^2)^0.5)
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
                  packages = NULL,
                  export = NULL,
                  ...)
{
  errorhandling <- match.arg(errorhandling)

  n_iter <- length(args)
  
  # progress bars
  if (!is.null(cl)) {
    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)
    `%op%` <- foreach::`%dopar%`
  } else {
    `%op%` <- foreach::`%do%`
  }
  
  if (show_progress)
  {
    pb <- txtProgressBar(min = 0,
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
    .packages = packages,
    .combine = combine,
    .errorhandling = errorhandling,
    .options.snow = opts,
    .verbose = verbose,
    .export = export
  ) %op% {
    if (show_progress)
    {
      setTxtProgressBar(pb, i)
    }
    do.call(what = what,
            args = c(list(args[i]), ...))
  }
  
  if (show_progress)
  {
    close(pb)
  }
  
  if (!is.null(cl))
  {
    snow::stopCluster(cl_SOCK)
  }
  
  return(res)
}

# Quantile split conformal prediction -----
quantile_scp <- function(abs_residuals, alpha) {
    n_cal_points <- length(abs_residuals)
    k <- ceiling((0.5 * n_cal_points + 1) * (1 - alpha))
    return(rank(abs_residuals)[k])
}

# Simulate Gaussian density -----
# Gaussian kernel density simulation
rgaussiandens <- function(x, n=length(x), p = 1, seed=1324) {  
  z <- stats::density(x, bw="sj", kernel="gaussian")
  width <- z$bw                              # Kernel width
  rkernel <- function(n, seed) {set.seed(seed); rnorm(n, sd=width)}  # Kernel sampler
  if (p <= 1)
  {                
    set.seed(seed)
    return(sample(x, n, replace=TRUE) + rkernel(n, seed))    # Here's the entire algorithm
  } else {
    return(sapply(1:p, 
    function(i) {set.seed(seed + i - 1); sample(x, n, replace=TRUE) + rkernel(n, seed + i - 1)}))
  }  
}

# Split a dataset -----
split_data <- function(y, p = 0.5, seed = 123) {
    # from caret::createFolds
    set.seed(seed)
    stopifnot((p > 0) && (p < 1))
    k <- floor(1/p)
    return_list <- TRUE
    returnTrain <- FALSE

    if (inherits(y, "Surv"))
        y <- y[, "time"]
    if (is.numeric(y)) {
        cuts <- floor(length(y)/k)
        if (cuts < 2)
            cuts <- 2
        if (cuts > 5)
            cuts <- 5
        breaks <- unique(stats::quantile(y, probs = seq(0, 1, length = cuts)))
        y <- cut(y, breaks, include.lowest = TRUE)
    }
    if (k < length(y)) {
        y <- factor(as.character(y))
        numInClass <- table(y)
        foldVector <- vector(mode = "integer", length(y))
        for (i in 1:length(numInClass)) {
            min_reps <- numInClass[i]%/%k
            if (min_reps > 0) {
                spares <- numInClass[i]%%k
                seqVector <- rep(1:k, min_reps)
                if (spares > 0)
                  seqVector <- c(seqVector, sample(1:k, spares))
                foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
            } else {
                foldVector[which(y == names(numInClass)[i])] <- sample(1:k, size = numInClass[i])
            }
        }
    } else {
        foldVector <- seq(along = y)
    }

    out <- split(seq(along = y), foldVector)

    return(out[[1]])
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
