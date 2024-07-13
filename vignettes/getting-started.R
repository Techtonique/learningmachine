## -----------------------------------------------------------------------------
library(learningmachine)
library(caret)

## -----------------------------------------------------------------------------
X <- as.matrix(mtcars[,-1])
y <- mtcars$mpg

## -----------------------------------------------------------------------------
set.seed(123)
(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]
dim(X_train)
dim(X_test)

## -----------------------------------------------------------------------------
obj <- learningmachine::BaseRegressor$new()

## -----------------------------------------------------------------------------
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## -----------------------------------------------------------------------------
print(sqrt(mean((obj$predict(X_test) - y_test)^2)))

## ----fig.width=7.2------------------------------------------------------------
res <- obj$predict(X = X_test, level = 95, method = "splitconformal")

plot(c(y_train, res$preds), type='l',
     main="",
     ylab="",
     ylim = c(min(c(res$upper, res$lower, y)),
              max(c(res$upper, res$lower, y))))
lines(c(y_train, res$upper), col="gray60")
lines(c(y_train, res$lower), col="gray60")
lines(c(y_train, res$preds), col = "red")
lines(c(y_train, y_test), col = "blue")

mean((y_test >= as.numeric(res$lower)) * (y_test <= as.numeric(res$upper)))

## ----fig.width=7.2------------------------------------------------------------
res <- obj$predict(X = X_test, level = 95, 
                   method = "jackknifeplus")

plot(c(y_train, res$preds), type='l',
     main="",
     ylab="",
     ylim = c(min(c(res$upper, res$lower, y)),
              max(c(res$upper, res$lower, y))))
lines(c(y_train, res$upper), col="gray60")
lines(c(y_train, res$lower), col="gray60")
lines(c(y_train, res$preds), col = "red")
lines(c(y_train, y_test), col = "blue")

mean((y_test >= as.numeric(res$lower)) * (y_test <= as.numeric(res$upper)))

## -----------------------------------------------------------------------------
obj <- learningmachine::RangerRegressor$new()
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## -----------------------------------------------------------------------------
print(sqrt(mean((obj$predict(X_test) - y_test)^2)))

## ----fig.width=7.2------------------------------------------------------------
res <- obj$predict(X = X_test, level = 95, method = "splitconformal")

plot(c(y_train, res$preds), type='l',
     main="",
     ylab="",
     ylim = c(min(c(res$upper, res$lower, y)),
              max(c(res$upper, res$lower, y))))
lines(c(y_train, res$upper), col="gray60")
lines(c(y_train, res$lower), col="gray60")
lines(c(y_train, res$preds), col = "red")
lines(c(y_train, y_test), col = "blue")

mean((y_test >= as.numeric(res$lower)) * (y_test <= as.numeric(res$upper)))

## ----fig.width=7.2------------------------------------------------------------
res <- obj$predict(X = X_test, level = 95, method = "jackknifeplus")

plot(c(y_train, res$preds), type='l',
     main="",
     ylab="",
     ylim = c(min(c(res$upper, res$lower, y)),
              max(c(res$upper, res$lower, y))))
lines(c(y_train, res$upper), col="gray60")
lines(c(y_train, res$lower), col="gray60")
lines(c(y_train, res$preds), col = "red")
lines(c(y_train, y_test), col = "blue")

mean((y_test >= as.numeric(res$lower)) * (y_test <= as.numeric(res$upper)))

## -----------------------------------------------------------------------------
obj <- learningmachine::XgboostRegressor$new()
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train, nrounds = 10L,
        verbose = 0,
        params = list(max_depth = 3L,
        eta = 0.1,
        subsample = 0.8,
        colsample_bytree = 0.8,
        objective = "reg:squarederror"))
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## ----echo=TRUE, fig.width=7.2-------------------------------------------------
res <- obj$predict(X = X_test, level = 95, method = "splitconformal")

plot(c(y_train, res$preds), type='l',
     main="",
     ylab="",
     ylim = c(min(c(res$upper, res$lower, y)),
              max(c(res$upper, res$lower, y))))
lines(c(y_train, res$upper), col="gray60")
lines(c(y_train, res$lower), col="gray60")
lines(c(y_train, res$preds), col = "red")
lines(c(y_train, y_test), col = "blue")

mean((y_test >= as.numeric(res$lower)) * (y_test <= as.numeric(res$upper)))

## -----------------------------------------------------------------------------
print(sqrt(mean((obj$predict(X_test) - y_test)^2)))

## ----echo=TRUE, fig.width=7.2-------------------------------------------------
res <- obj$predict(X = X_test, level = 95, method = "jackknifeplus")

plot(c(y_train, res$preds), type='l',
     main="",
     ylab="",
     ylim = c(min(c(res$upper, res$lower, y)),
              max(c(res$upper, res$lower, y))))
lines(c(y_train, res$upper), col="gray60")
lines(c(y_train, res$lower), col="gray60")
lines(c(y_train, res$preds), col = "red")
lines(c(y_train, y_test), col = "blue")

mean((y_test >= as.numeric(res$lower)) * (y_test <= as.numeric(res$upper)))

## -----------------------------------------------------------------------------
# Boston dataset (dataset has an ethical problem)
library(MASS)
data("Boston")

set.seed(1234)
train_idx <- sample(nrow(Boston), 0.8 * nrow(Boston))
X_train <- as.matrix(Boston[train_idx, -ncol(Boston)])
X_test <- as.matrix(Boston[-train_idx, -ncol(Boston)])
y_train <- Boston$medv[train_idx]
y_test <- Boston$medv[-train_idx]

obj <- learningmachine::BcnRegressor$new()

t0 <- proc.time()[3]
obj$fit(X = X_train, y = y_train, B = 500L, nu = 0.5646811,
lam = 10**0.5106108, r = 1 - 10**(-7), tol = 10**-7,
col_sample = 0.5, activation = "tanh", type_optim = "nlminb", 
show_progress = FALSE)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## -----------------------------------------------------------------------------
print(sqrt(mean((obj$predict(X_test) - y_test)^2)))

## -----------------------------------------------------------------------------
obj <- learningmachine::XgboostRegressor$new()
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train, nrounds = 500L,
        verbose = 0,
        params = list(objective = "reg:squarederror"))
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## -----------------------------------------------------------------------------
print(sqrt(mean((obj$predict(X_test) - y_test)^2)))

## -----------------------------------------------------------------------------
obj <- learningmachine::KernelRidgeRegressor$new()
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train, reg_lambda = 0.1)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## -----------------------------------------------------------------------------
print(sqrt(mean((obj$predict(X_test) - y_test)^2)))

## -----------------------------------------------------------------------------
obj <- learningmachine::RangerRegressor$new()
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## -----------------------------------------------------------------------------
print(sqrt(mean((obj$predict(X_test) - y_test)^2)))

## -----------------------------------------------------------------------------
X <- as.matrix(mtcars[,-1])
y <- mtcars$mpg

set.seed(123)
(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.7*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]
dim(X_train)
dim(X_test)

## -----------------------------------------------------------------------------
obj <- learningmachine::KernelRidgeRegressor$new()
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train, reg_lambda = 0.1)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## -----------------------------------------------------------------------------
print(sqrt(mean((obj$predict(X_test) - y_test)^2)))

## ----fig.width=7.2------------------------------------------------------------
res <- obj$predict(X = X_test, level = 95, method = "splitconformal")

plot(c(y_train, res$preds), type='l',
     main="",
     ylab="",
     ylim = c(min(c(res$upper, res$lower, y)),
              max(c(res$upper, res$lower, y))))
lines(c(y_train, res$upper), col="gray60")
lines(c(y_train, res$lower), col="gray60")
lines(c(y_train, res$preds), col = "red")
lines(c(y_train, y_test), col = "blue")

mean((y_test >= as.numeric(res$lower)) * (y_test <= as.numeric(res$upper)))

## ----fig.width=7.2------------------------------------------------------------
res <- obj$predict(X = X_test, level = 95, method = "jackknifeplus")

plot(c(y_train, res$preds), type='l',
     main="",
     ylab="",
     ylim = c(min(c(res$upper, res$lower, y)),
              max(c(res$upper, res$lower, y))))
lines(c(y_train, res$upper), col="gray60")
lines(c(y_train, res$lower), col="gray60")
lines(c(y_train, res$preds), col = "red")
lines(c(y_train, y_test), col = "blue")

mean((y_test >= as.numeric(res$lower)) * (y_test <= as.numeric(res$upper)))

## -----------------------------------------------------------------------------
set.seed(123)
X <- as.matrix(iris[, 1:4])
y <- iris$Species

(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]
dim(X_train)
dim(X_test)

## -----------------------------------------------------------------------------
obj <- learningmachine::RangerClassifier$new()
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

mean(obj$predict(X_test) == y_test)

## -----------------------------------------------------------------------------
print(obj$predict_proba(X_test))

## -----------------------------------------------------------------------------
caret::confusionMatrix(data = obj$predict(X_test), reference = y_test)

## -----------------------------------------------------------------------------
data(iris)
X <- as.matrix(iris[, 1:4])
print(head(X))
y <- iris$Species
print(head(y))

set.seed(1214)
(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]
dim(X_train)
dim(X_test)

## -----------------------------------------------------------------------------
obj <- learningmachine::XgboostClassifier$new()
obj$get_type()
obj$get_name()

## -----------------------------------------------------------------------------
t0 <- proc.time()[3]
obj$fit(X_train, y_train, nrounds = 10L,
        params = list(max_depth = 3L,
                      eta = 0.1,
                      subsample = 0.8,
                      colsample_bytree = 0.8,
                      objective = "multi:softprob",
                      num_class = 3L))
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

## ----echo=TRUE----------------------------------------------------------------
print(mean(obj$predict(X_test) == y_test))

## ----echo=TRUE----------------------------------------------------------------
print(obj$predict_proba(X_test))

## -----------------------------------------------------------------------------
caret::confusionMatrix(data = obj$predict(X_test), reference = y_test)

## -----------------------------------------------------------------------------
# iris dataset
set.seed(1234)
train_idx <- sample(nrow(iris), 0.8 * nrow(iris))
X_train <- as.matrix(iris[train_idx, -ncol(iris)])
X_test <- as.matrix(iris[-train_idx, -ncol(iris)])
y_train <- iris$Species[train_idx]
y_test <- iris$Species[-train_idx]

obj <- learningmachine::BcnClassifier$new()

t0 <- proc.time()[3]
obj$fit(X = X_train, y = y_train, 
        B = 10L, nu = 0.335855,
        lam = 10**0.7837525, 
        r = 1 - 10**(-5.470031), 
        tol = 10**-7,
        activation = "tanh", 
        type_optim = "nlminb", 
        show_progress = FALSE)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

mean(obj$predict(X_test) == y_test)

## -----------------------------------------------------------------------------
caret::confusionMatrix(data = obj$predict(X_test), reference = y_test)

## -----------------------------------------------------------------------------
set.seed(123)
X <- as.matrix(iris[, 1:4])
# y <- factor(as.numeric(iris$Species))
y <- iris$Species

index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE)
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]
dim(X_train)
dim(X_test)

obj <- learningmachine::KernelRidgeClassifier$new()
obj$get_type()
obj$get_name()

t0 <- proc.time()[3]
obj$fit(X_train, y_train)
cat("Elapsed: ", proc.time()[3] - t0, "s \n")

mean(obj$predict(X_test) == y_test)

## -----------------------------------------------------------------------------
caret::confusionMatrix(data = obj$predict(X_test), reference = y_test)

