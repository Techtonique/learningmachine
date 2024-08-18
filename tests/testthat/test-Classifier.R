# data(iris)

# X <- as.matrix(iris[,-5])
# y <- iris$Species

# set.seed(123)
# (index_train <- base::sample.int(n = nrow(X),
#                                  size = floor(0.8*nrow(X)),
#                                  replace = FALSE))
# X_train <- X[index_train, ]
# y_train <- y[index_train]
# X_test <- X[-index_train, ]
# y_test <- y[-index_train]

# obj1 <- learningmachine::Classifier$new(method = "lm")
# obj2 <- learningmachine::Classifier$new(method = "ranger")
# obj3 <- learningmachine::Classifier$new(method = "extratrees")
# obj4 <- learningmachine::Classifier$new(method = "ridge")
# obj5 <- learningmachine::Classifier$new(method = "bcn")
# obj6 <- learningmachine::Classifier$new(method = "glmnet")
# obj7 <- learningmachine::Classifier$new(method = "krr")
# obj8 <- learningmachine::Classifier$new(method = "xgboost")
# obj9 <- learningmachine::Classifier$new(method = "lm", nb_hidden = 3)
# obj10 <- learningmachine::Classifier$new(method = "ranger", nb_hidden = 3)
# obj11 <- learningmachine::Classifier$new(method = "extratrees", nb_hidden = 3)
# obj12 <- learningmachine::Classifier$new(method = "ridge", nb_hidden = 3)
# obj13 <- learningmachine::Classifier$new(method = "bcn", nb_hidden = 3)
# obj14 <- learningmachine::Classifier$new(method = "glmnet", nb_hidden = 3)
# obj15 <- learningmachine::Classifier$new(method = "krr", nb_hidden = 3)
# obj16 <- learningmachine::Classifier$new(method = "xgboost", nb_hidden = 3)
# obj17 <- learningmachine::Classifier$new(method = "ranger", 
#                                         pi_method="kdesplitconformal")
# obj18 <- learningmachine::Classifier$new(method = "ranger", 
#                                         nb_hidden = 3, 
#                                         pi_method="kdesplitconformal")

# (obj1$get_type())
# (obj1$get_name())
# (obj1$get_method())
# (obj2$get_method())
# (obj3$get_method())
# (obj4$get_method())
# (obj5$get_method())
# (obj6$get_method())
# (obj7$get_method())
# (obj8$get_method())

# obj1$fit(X_train, y_train)
# obj2$fit(X_train, y_train)
# obj3$fit(X_train, y_train)
# obj4$fit(X_train, y_train)
# obj5$fit(X_train, y_train, 
#          show_progress=FALSE)
# obj6$fit(X_train, y_train, lambda=0.01)
# obj7$fit(X_train, y_train)
# obj8$fit(X_train, y_train, nrounds=10, verbose=FALSE)
# obj9$fit(X_train, y_train)
# obj10$fit(X_train, y_train)
# obj11$fit(X_train, y_train)
# obj12$fit(X_train, y_train)
# obj13$fit(X_train, y_train, 
#          show_progress=FALSE)
# obj14$fit(X_train, y_train, lambda=0.01)
# obj15$fit(X_train, y_train)
# obj16$fit(X_train, y_train, nrounds=10, verbose=FALSE)
# obj17$fit(X_train, y_train)
# obj18$fit(X_train, y_train)



# (acc1 <- mean((obj1$predict(X_test) == y_test)))
# (acc2 <- mean((obj2$predict(X_test) == y_test)))
# (acc3 <- mean((obj3$predict(X_test) == y_test)))
# (acc4 <- mean((obj4$predict(X_test) == y_test)))
# (acc5 <- mean((obj5$predict(X_test) == y_test)))
# (acc6 <- mean((obj6$predict(X_test) == y_test)))
# (acc7 <- mean((obj7$predict(X_test) == y_test)))
# (acc8 <- mean((obj8$predict(X_test) == y_test)))
# (acc9 <- mean((obj9$predict(X_test) == y_test)))
# (acc10 <- mean((obj10$predict(X_test) == y_test)))
# (acc11 <- mean((obj11$predict(X_test) == y_test)))
# (acc12 <- mean((obj12$predict(X_test) == y_test)))
# (acc13 <- mean((obj13$predict(X_test) == y_test)))
# (acc14 <- mean((obj14$predict(X_test) == y_test)))
# (acc15 <- mean((obj15$predict(X_test) == y_test)))
# (acc16 <- mean((obj16$predict(X_test) == y_test)))

# (prob1 <- obj1$predict_proba(X_test)[1, 1])
# (prob2 <- obj2$predict_proba(X_test)[1, 1])
# (prob3 <- obj3$predict_proba(X_test)[1, 1])
# (prob4 <- obj4$predict_proba(X_test)[1, 1])
# (prob5 <- obj5$predict_proba(X_test)[1, 1])
# (prob6 <- obj6$predict_proba(X_test)[1, 1])
# (prob7 <- obj7$predict_proba(X_test)[1, 1])
# (prob8 <- obj8$predict_proba(X_test)[1, 1])
# (prob9 <- obj9$predict_proba(X_test)[1, 1])
# (prob10 <- obj10$predict_proba(X_test)[1, 1])
# (prob11 <- obj11$predict_proba(X_test)[1, 1])
# (prob12 <- obj12$predict_proba(X_test)[1, 1])
# (prob13 <- obj13$predict_proba(X_test)[1, 1])
# (prob14 <- obj14$predict_proba(X_test)[1, 1])
# (prob15 <- obj15$predict_proba(X_test)[1, 1])
# (prob16 <- obj16$predict_proba(X_test)[1, 1])

# test_that("1 - checks on basic getters", {
#   expect_equal(obj1$get_type(), "classification")
#   expect_equal(obj1$get_name(), "Classifier")
#   expect_equal(obj1$get_method(), "lm")
#   expect_equal(obj2$get_method(), "ranger")
#   expect_equal(obj3$get_method(), "extratrees")
#   expect_equal(obj4$get_method(), "ridge")
#   expect_equal(obj5$get_method(), "bcn")
#   expect_equal(obj6$get_method(), "glmnet")
#   expect_equal(obj7$get_method(), "krr")
#   expect_equal(obj8$get_method(), "xgboost")
# })

# test_that("2 - checks on basic fitting", {
#   expect_equal(round(acc1, 2), 0.77)
#   expect_equal(round(acc2, 2), 0.97)
#   expect_equal(round(acc3, 2), 0.97)
#   expect_equal(round(acc4, 2), 0.8)
#   expect_equal(round(acc5, 2), 0.17)
#   expect_equal(round(acc6, 2), 0.73)
#   expect_equal(round(acc7, 2), 0.97)
#   expect_equal(round(acc8, 2), 0.97)
# })

# test_that("3 - basic checks on regression-based probas", {
#   expect_equal(as.numeric(round(prob1, 2)), 0.42)
#   expect_equal(as.numeric(round(prob2, 2)), 0.42)
#   expect_equal(as.numeric(round(prob3, 2)), 0.42)
#   expect_equal(as.numeric(round(prob4, 2)), 0.42)
#   expect_equal(as.numeric(round(prob5, 2)), 0.33)
#   expect_equal(as.numeric(round(prob6, 2)), 0.42)
#   expect_equal(as.numeric(round(prob7, 2)), 0.42)
#   expect_equal(as.numeric(round(prob8, 2)), 0.42)
# })


# test_that("3 - checks on fitting qrns", {
#   expect_equal(round(acc9, 2), 0.77)
#   expect_equal(round(acc10, 2), 0.97)
#   expect_equal(round(acc11, 2), 0.97)
#   expect_equal(round(acc12, 2), 0.8)
#   expect_equal(round(acc13, 2), 0.17)
#   expect_equal(round(acc14, 2), 0.73)
#   expect_equal(round(acc15, 2), 0.97)
#   expect_equal(round(acc16, 2), 0.97)
# })

# test_that("4 - checks on regression-based probas for qrns", {
#   expect_equal(as.numeric(round(prob9, 2)), 0.42)
#   expect_equal(as.numeric(round(prob10, 2)), 0.42)
#   expect_equal(as.numeric(round(prob11, 2)), 0.42)
#   expect_equal(as.numeric(round(prob12, 2)), 0.42)
#   expect_equal(as.numeric(round(prob13, 2)), 0.33)
#   expect_equal(as.numeric(round(prob14, 2)), 0.42)
#   expect_equal(as.numeric(round(prob15, 2)), 0.42)
#   expect_equal(as.numeric(round(prob16, 2)), 0.42)
# })

# # test_that("5 - conformal no sims", { 
# #   expect_equal(round(cv1, 2), 1)
# #   expect_equal(round(cv2, 2), 1)
# #   expect_equal(round(cv3, 2), 0.57)
# #   expect_equal(round(cv4, 2), 1)
# # })
