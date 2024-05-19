data(mtcars)

X <- as.matrix(mtcars[,-1])
y <- mtcars$mpg

set.seed(123)
(index_train <- base::sample.int(n = nrow(X),
                                 size = floor(0.8*nrow(X)),
                                 replace = FALSE))
X_train <- X[index_train, ]
y_train <- y[index_train]
X_test <- X[-index_train, ]
y_test <- y[-index_train]

obj1 <- learningmachine::Regressor$new(method = "lm")
obj2 <- learningmachine::Regressor$new(method = "ranger")
obj3 <- learningmachine::Regressor$new(method = "extratrees")
obj4 <- learningmachine::Regressor$new(method = "ridge")
obj5 <- learningmachine::Regressor$new(method = "bcn")
obj6 <- learningmachine::Regressor$new(method = "glmnet")
obj7 <- learningmachine::Regressor$new(method = "krr")
obj8 <- learningmachine::Regressor$new(method = "xgboost")

(obj1$get_type())
(obj1$get_name())
(obj1$get_method())
(obj2$get_method())
(obj3$get_method())
(obj4$get_method())
(obj5$get_method())
(obj6$get_method())
(obj7$get_method())
(obj8$get_method())

obj1$fit(X_train, y_train)
obj2$fit(X_train, y_train)
obj3$fit(X_train, y_train)
obj4$fit(X_train, y_train)
obj5$fit(X_train, y_train, 
         show_progress=FALSE)
obj6$fit(X_train, y_train)
obj7$fit(X_train, y_train, lambda=0.05)
obj8$fit(X_train, y_train, 
         nrounds=10, verbose=FALSE)

(mse1 <- mean((obj1$predict(X_test) - y_test)^2))
(mse2 <- mean((obj2$predict(X_test) - y_test)^2))
(mse3 <- mean((obj3$predict(X_test) - y_test)^2))
(mse4 <- mean((obj4$predict(X_test) - y_test)^2))
(mse5 <- mean((obj5$predict(X_test) - y_test)^2))
(mse6 <- mean((obj6$predict(X_test) - y_test)^2))
(mse7 <- mean((obj7$predict(X_test) - y_test)^2))
(mse8 <- mean((obj8$predict(X_test) - y_test)^2))


test_that("1 - checks on basic getters", {
  expect_equal(obj1$get_type(), "regression")
  expect_equal(obj1$get_name(), "Regressor")
  expect_equal(obj1$get_method(), "lm")
  expect_equal(obj2$get_method(), "ranger")
  expect_equal(obj3$get_method(), "extratrees")
  expect_equal(obj4$get_method(), "ridge")
  expect_equal(obj5$get_method(), "bcn")
  expect_equal(obj6$get_method(), "glmnet")
  expect_equal(obj7$get_method(), "krr")
  expect_equal(obj8$get_method(), "xgboost")
})

test_that("2 - checks on basic fitting", {
  expect_equal(round(mse1, 2), 12.59)
  expect_equal(round(mse2, 2), 5.5)
  expect_equal(round(mse3, 2), 6.98)
  expect_equal(round(mse4, 2), 5.78)
  expect_equal(round(mse5, 2), 18.93)
  expect_equal(round(mse6, 2), 10.07)
  expect_equal(round(mse7, 2), 4.99)
  expect_equal(round(mse8, 2), 5.52)
})


test_that("3 - additional parameters", {
})
