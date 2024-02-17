
# 1 - Class Base --------------------------------------------------------------

Base <-
  R6::R6Class(
    classname = "Base",
    private = list(
      encoded_factors = NULL,
      class_names = NULL,
      n_classes = NULL,
      y = NULL,
      type_split = NULL,
      calib_resids = NULL,
      abs_calib_resids = NULL
    ),
    public = list(
      name = "Base",
      type = "none",
      model = NULL,
      method = NULL,
      X_train = NULL,
      y_train = NULL,
      pi_method = NULL,
      level = NULL,
      B = NULL,
      engine = NULL,
      params = NULL,
      seed = 123,
      initialize = function(name = "Base",
                            type = "none",
                            model = NULL,
                            method = NULL,
                            X_train = NULL,
                            y_train = NULL,
                            pi_method = NULL,
                            level = NULL,
                            B = NULL,
                            engine = NULL,
                            params = NULL,
                            seed = 123) {
        self$name <- name
        self$type <- type
        self$model <- model
        self$method <- method
        self$X_train <- X_train
        self$y_train <- y_train
        self$pi_method <- pi_method
        self$level <- level
        self$B <- B
        self$engine <- engine
        self$params <- params
        self$seed <- seed
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
      },
      get_method = function() {
        self$method
      },
      set_method = function(method) {
        self$method <- method
      },
      get_pi_method = function() {
        self$pi_method
      },
      set_pi_method = function(pi_method) {
        self$pi_method <- pi_method
      },
      get_level = function() {
        self$level
      },
      set_level = function(level) {
        self$level <- level
      },
      get_B = function() {
        self$B
      },
      set_B = function(B) {
        self$B <- B
      },
      set_engine = function(engine) {
        self$engine <- engine
      },
      get_engine = function() {
        self$engine
      },
      get_params = function() {
        self$params
      },
      get_seed = function() {
        self$seed
      },
      set_seed = function(seed) {
        self$seed <- seed
      }
    )
  )
