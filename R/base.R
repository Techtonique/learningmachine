

# 1 - BaseRegressor -----------------------------------------------------------

BaseRegressor <- R6::R6Class("BaseRegressor",
                         public = list(
                           name = "BaseRegressor",
                           type = "regression",
                           model = NULL,
                           initialize = function(name = "BaseRegressor",
                                                 type = "regression",
                                                 model = NULL) {
                             self$name <- name
                             self$type <- type
                             self$model <- model
                           },
                           get_name = function() {
                             self$name
                           },
                           get_type = function() {
                             self$type
                           },
                           get_model = function() {
                             self$model
                           },
                           set_model = function(model) {
                             self$model <- model
                           }))

# 2 - BaseClassifier -----------------------------------------------------------

BaseClassifier <- R6::R6Class(classname = "BaseClassifier",
                          public = list(
                            name = "BaseClassifier",
                            type = "classification",
                            model = NULL,
                            initialize = function(name = "BaseClassifier",
                                                  type = "classification",
                                                  model = NULL) {
                              self$name <- name
                              self$type <- type
                              self$model <- model
                            },
                            get_name = function() {
                              return(self$name)
                            },
                            get_type = function() {
                              return(self$type)
                            },
                            get_model = function() {
                              self$model
                            },
                            set_model = function(model) {
                              self$model <- model
                            }))
