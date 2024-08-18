# data(mtcars)

# X <- as.matrix(mtcars[,-1])
# y <- mtcars$mpg

# set.seed(123)
# (index_train <- base::sample.int(n = nrow(X),
#                                  size = floor(0.8*nrow(X)),
#                                  replace = FALSE))
# X_train <- X[index_train, ]
# y_train <- y[index_train]
# X_test <- X[-index_train, ]
# y_test <- y[-index_train]

# obj1 <- learningmachine::Regressor$new(method = "lm")
# obj2 <- learningmachine::Regressor$new(method = "ranger")
# obj3 <- learningmachine::Regressor$new(method = "extratrees")
# obj4 <- learningmachine::Regressor$new(method = "ridge")
# obj5 <- learningmachine::Regressor$new(method = "bcn")
# obj6 <- learningmachine::Regressor$new(method = "glmnet")
# obj7 <- learningmachine::Regressor$new(method = "krr")
# obj8 <- learningmachine::Regressor$new(method = "xgboost")
# obj10 <- learningmachine::Regressor$new(method = "lm", nb_hidden = 3)
# obj11 <- learningmachine::Regressor$new(method = "ranger", nb_hidden = 3)
# obj12 <- learningmachine::Regressor$new(method = "extratrees", nb_hidden = 3)
# obj13 <- learningmachine::Regressor$new(method = "ridge", nb_hidden = 3)
# #obj14 <- learningmachine::Regressor$new(method = "bcn", nb_hidden = 3)
# obj15 <- learningmachine::Regressor$new(method = "glmnet", nb_hidden = 3)
# obj16 <- learningmachine::Regressor$new(method = "krr", nb_hidden = 3)
# obj17 <- learningmachine::Regressor$new(method = "xgboost", nb_hidden = 3)
# obj18 <- learningmachine::Regressor$new(method = "lm", 
#                                         pi_method="splitconformal")
# obj19 <- learningmachine::Regressor$new(method = "ranger", 
#                                         pi_method="jackknifeplus")
# obj20 <- learningmachine::Regressor$new(method = "lm", 
#                                         nb_hidden = 3, 
#                                         pi_method="splitconformal")
# obj21 <- learningmachine::Regressor$new(method = "ranger", 
#                                         nb_hidden = 3, 
#                                         pi_method="jackknifeplus")
# obj22 <- learningmachine::Regressor$new(method = "svm", 
#                                         nb_hidden=3,
#                                         pi_method="splitconformal")
# obj23 <- learningmachine::Regressor$new(method = "svm", 
#                                         nb_hidden=3,
#                                         pi_method="jackknifeplus")
# obj24 <- learningmachine::Regressor$new(method = "svm", 
#                                         nb_hidden=3,
#                                         pi_method="kdesplitconformal")
# obj25 <- learningmachine::Regressor$new(method = "svm", 
#                                         nb_hidden=3,
#                                         pi_method="kdejackknifeplus")




# (params_qrn <- expand.grid(nodes_sim = c("sobol", "halton", "unif"), 
#             activ=c("relu", "sigmoid", "tanh", 
#                     "leakyrelu", "elu", "linear")))
# params_qrn$nodes_sim <- as.vector(params_qrn$nodes_sim)
# params_qrn$activ <- as.vector(params_qrn$activ)

# obj9 <- learningmachine::Regressor$new(method = "ranger", 
#                                        nb_hidden=5)


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
# obj6$fit(X_train, y_train)
# obj7$fit(X_train, y_train, reg_lambda=0.05)
# obj8$fit(X_train, y_train, 
#          nrounds=10, verbose=FALSE)
# obj9$fit(X_train, y_train)
# obj10$fit(X_train, y_train)
# obj11$fit(X_train, y_train)
# obj12$fit(X_train, y_train)
# obj13$fit(X_train, y_train)
# #obj14$fit(X_train, y_train, show_progress=FALSE)
# obj15$fit(X_train, y_train)
# obj16$fit(X_train, y_train, reg_lambda=0.05)
# obj17$fit(X_train, y_train, 
#          nrounds=10, verbose=FALSE)
# obj18$fit(X_train, y_train)
# obj19$fit(X_train, y_train)
# obj20$fit(X_train, y_train)
# obj21$fit(X_train, y_train)
# obj22$fit(X_train, y_train, 
#           type_split="sequential")
# obj23$fit(X_train, y_train)
# obj24$fit(X_train, y_train, 
#           type_split="sequential")
# obj25$fit(X_train, y_train)

# (mse1 <- mean((obj1$predict(X_test) - y_test)^2))
# (mse2 <- mean((obj2$predict(X_test) - y_test)^2))
# (mse3 <- mean((obj3$predict(X_test) - y_test)^2))
# (mse4 <- mean((obj4$predict(X_test) - y_test)^2))
# (mse5 <- mean((obj5$predict(X_test) - y_test)^2))
# (mse6 <- mean((obj6$predict(X_test) - y_test)^2))
# (mse7 <- mean((obj7$predict(X_test) - y_test)^2))
# (mse8 <- mean((obj8$predict(X_test) - y_test)^2))
# (mse9 <- mean((obj9$predict(X_test) - y_test)^2))
# (mse10 <- mean((obj10$predict(X_test) - y_test)^2))
# (mse11 <- mean((obj11$predict(X_test) - y_test)^2))
# (mse12 <- mean((obj12$predict(X_test) - y_test)^2))
# (mse13 <- mean((obj13$predict(X_test) - y_test)^2))
# #(mse14 <- mean((obj14$predict(X_test) - y_test)^2))
# (mse15 <- mean((obj15$predict(X_test) - y_test)^2))
# (mse16 <- mean((obj16$predict(X_test) - y_test)^2))
# (mse17 <- mean((obj17$predict(X_test) - y_test)^2))
# preds_obj18 <- obj18$predict(X_test)
# preds_obj22 <- obj22$predict(X_test)
# preds_obj23 <- obj23$predict(X_test)
# (cv1 <- mean((preds_obj18$lower <= y_test)*(preds_obj18$upper >= y_test)))
# preds_obj19 <- obj19$predict(X_test)
# (cv2 <- mean((preds_obj19$lower <= y_test)*(preds_obj19$upper >= y_test)))
# preds_obj20 <- obj20$predict(X_test)
# (cv3 <- mean((preds_obj20$lower <= y_test)*(preds_obj20$upper >= y_test)))
# preds_obj21 <- obj21$predict(X_test) 
# (cv4 <- mean((preds_obj21$lower <= y_test)*(preds_obj21$upper >= y_test)))
# preds_obj22 <- obj22$predict(X_test)
# (cv5 <- mean((preds_obj22$lower <= y_test)*(preds_obj22$upper >= y_test)))
# preds_obj23 <- obj23$predict(X_test) 
# (cv6 <- mean((preds_obj23$lower <= y_test)*(preds_obj23$upper >= y_test)))
# preds_obj24 <- obj24$predict(X_test)
# (cv7 <- mean((preds_obj24$lower <= y_test)*(preds_obj24$upper >= y_test)))
# preds_obj25 <- obj25$predict(X_test) 
# (cv8 <- mean((preds_obj25$lower <= y_test)*(preds_obj25$upper >= y_test)))


# mses_qrn <- rep(0, nrow(params_qrn))
# for (i in 1:length(mses_qrn))
# {
#   obj_temp <- learningmachine::Regressor$new(method = "ranger", 
#                                              nb_hidden = 3,
#                                              nodes_sim = params_qrn$nodes_sim[i],
#                                              activ = params_qrn$activ[i])
#   obj_temp$fit(X_train, y_train)
#   mses_qrn[i] <- mean((obj_temp$predict(X_test) - y_test)^2) 
# }


# test_that("1 - checks on basic getters", {
#   expect_equal(obj1$get_type(), "regression")
#   expect_equal(obj1$get_name(), "Regressor")
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
#   expect_equal(round(mse1, 2), 5.34)
#   expect_equal(round(mse2, 2), 4.66)
#   expect_equal(round(mse3, 2), 5.59)
#   expect_equal(round(mse4, 2), 4.72)
#   expect_equal(round(mse5, 2), 21.55)
#   expect_equal(round(mse6, 2), 6.42)
#   expect_equal(round(mse7, 2), 2.2)
#   expect_equal(round(mse8, 2), 6.11)
# })


# test_that("3 - checks qrn", {
#   expect_equal(round(mse9, 2), 4.83)
#   expect_equal(sum(round(mses_qrn, 2)), 83.31)
#   expect_equal(round(mse10, 2), 5.34)
#   expect_equal(round(mse11, 2), 4.82)
#   expect_equal(round(mse12, 2), 5.36)
#   expect_equal(round(mse13, 2), 4.72)
#   #expect_equal(round(mse14, 2), 21.55)
#   expect_equal(round(mse15, 2), 6.42)
#   expect_equal(round(mse16, 2), 2.2)
#   expect_equal(round(mse17, 2), 6.11)
# })


# test_that("4 - conformal", {
#   expect_equal(round(cv1, 2), 1)
#   expect_equal(round(cv2, 2), 1)
#   expect_equal(round(cv3, 2), 0.57)
#   expect_equal(round(cv4, 2), 1)
#   expect_equal(round(cv5, 2), 1)
#   expect_equal(round(cv6, 2), 1)
#   expect_equal(round(cv7, 2), 1)
#   expect_equal(round(cv8, 2), 1)
# })