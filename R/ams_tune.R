# Time tests -------------------------------------------------------------------
#### Original function, all vars ####
tictoc::tic()
ams_test_orig <- ams_tune_svm_orig(svm,
                                   Label ~ .,
                                   data = higgs_training,
                                   kernel = "radial",
                                   cross = 5,
                                   training_data_weights = training_weights,
                                   ranges = list(
                                     cost = c(0.5, 1, 2),
                                     gamma = c(0.01, 0.1, 0.5)
                                   ))
tictoc::toc() # 1.92 sec elapsed
ams_test_orig$best.parameters # cost = 2, gamma = 0.1

#### Parallel function, all vars ####
tictoc::tic()
ams_test_parallel <- ams_tune_svm_parallel(svm,
                                   Label ~ .,
                                   data = higgs_training,
                                   kernel = "radial",
                                   cross = 5,
                                   training_data_weights = training_weights,
                                   ranges = list(
                                     cost = c(0.5, 1, 2),
                                     gamma = c(0.01, 0.1, 0.5)
                                   ))
tictoc::toc() # 0.579 sec elapsed
ams_test_parallel$best.parameters # cost = 2, gamma = 0.1

#### Parallel function, minus uniform and collinear ####
tictoc::tic()
ams_test_parallel_drop_combo <- ams_tune_svm_parallel(svm,
                                                      Label ~ .,
                                                      data = higgs_training_drop_combo,
                                                      kernel = "radial",
                                                      cross = 5,
                                                      training_data_weights = training_weights,
                                                      ranges = list(
                                                        cost = c(0.5, 1, 2),
                                                        gamma = c(0.01, 0.1, 0.5)
                                                      ))
tictoc::toc() # 0.44 sec elapsed
ams_test_parallel_drop_combo$best.parameters # cost = 2, gamma = 0.1

# Functions --------------------------------------------------------------------
ams_tune_svm_parallel <- function(METHOD, train.x, train.y = NULL, data = list(),
                                  training_data_weights = NULL,
                                  validation.x = NULL, validation.y = NULL,
                                  ranges = NULL, predict.func = predict,
                                  cross = 5,
                                  ...
) {
  call <- match.call()
  
  # Set up parallelisation
  n_cores <- parallel::detectCores()
  doParallel::registerDoParallel(n_cores * 0.8)
  
  ## internal helper functions
  resp <- function(formula, data) {
    
    model.response(model.frame(formula, data))
  }
  
  # Function for extracting the data for validation
  getValidationData <- function(train.x, validation.x, useFormula, data, train_ind_sample) {
    if (!is.null(validation.x)) {
      return(validation.x)
    } else if (useFormula) {
      return(data[-train_ind_sample,, drop = FALSE])
    } else if (inherits(train.x, "matrix.csr")) {
      return(train.x[-train_ind_sample, ])
    } else {
      return(train.x[-train_ind_sample,, drop = FALSE])
    }
  }
  
  # Function for extracting true y values
  getTrueY <- function(validation.y, useFormula, train.x, data, train_ind_sample) {
    if (!is.null(validation.y)) {
      return(validation.y)
    } else if (useFormula) {
      if (!is.null(validation.x)) {
        return(resp(train.x, validation.x))
      } else {
        return(resp(train.x, data[-train_ind_sample,]))
      }
    } else {
      return(train.y[-train_ind_sample])
    }
  }
  
  # Function for computing performance metric
  computeAMS <- function(true.y, pred, data, weights, train_ind_sample) {
    adj_weights <- adjust_weights(cbind(data, weights),
                                  cbind(data, weights)[train_ind_sample, ],
                                  unadjusted_weight_col = as.character(quote(weights)))
    ams <- approx_median_sig(predictions = pred,
                             labels = true.y,
                             weights = adj_weights,
                             b_reg = 10)
    return(ams)
  }
  
  ## parameter handling
  validation.x <- validation.y <- NULL
  useFormula <- is.null(train.y)
  if (useFormula && (is.null(data) || length(data) == 0))
    data <- model.frame(train.x)
  if (is.vector(train.x)) train.x <- t(t(train.x))
  if (is.data.frame(train.y))
    train.y <- as.matrix(train.y)
  
  ## prepare training indices
  n <- nrow(if (useFormula) data else train.x)
  perm.ind <- sample(n)
  if (cross > n)
    stop(sQuote("cross"), " must not exceed sampling size!")
  if (cross == 1)
    stop(sQuote("cross"), " must be greater than 1!")
  train.ind <- tapply(1:n, cut(1:n, breaks = cross), function(x) perm.ind[-x])
  
  ## find best model
  parameters <- if (is.null(ranges))
    data.frame(dummyparameter = 0)
  else
    expand.grid(ranges)
  p <- nrow(parameters)
  model.variances <- model.ams <- c()
  
  ## - loop over all models
  sampling.ams <- foreach(para.set = 1:p, .combine = "cbind") %:%
    foreach(sample = 1:length(train.ind), .combine = "c") %dopar% {
      pars <- if (is.null(ranges)) {
        NULL
      } else {
        lapply(parameters[para.set,,drop = FALSE], unlist)
      }
      
      train_data <- if (useFormula) {
        list(train.x, data = data, subset = train.ind[[sample]], ...)
      } else {
        list(train.x[train.ind[[sample]],], y = train.y[train.ind[[sample]]], ...)
      }
      
      # Train models and predict validation set
      model <- do.call(METHOD, c(train_data, pars))
      pred <- predict.func(model, getValidationData(train.x, validation.x, useFormula, data, train.ind[[sample]]))
      
      ## compute performance measure
      true.y <- getTrueY(validation.y, useFormula, train.x, data, train.ind[[sample]])
      
      if (is.null(true.y)) true.y <- rep(TRUE, length(models[[1]]))
      
      computeAMS(true.y, pred, data, training_data_weights, train.ind[[sample]])
    }
  
  # clean up the cluster
  doParallel::stopImplicitCluster()
  
  sampling.ams <- matrix(unlist(sampling.ams), nrow = length(train.ind), ncol = p)
  model.ams <- apply(sampling.ams, 2, mean)
  model.variances <- apply(sampling.ams, 2, sd)
  
  ## return results
  best <- which.max(model.ams)
  pars <- if (is.null(ranges))
    NULL
  else
    lapply(parameters[best,,drop = FALSE], unlist)
  structure(list(best.parameters  = parameters[best,,drop = FALSE],
                 best.performance = model.ams[best],
                 method           = if (!is.character(METHOD))
                   deparse(substitute(METHOD)) else METHOD,
                 nparcomb         = nrow(parameters),
                 train.ind        = train.ind,
                 sampling         = if (cross == n) "leave-one-out" else
                   paste(cross,"-fold cross validation", sep=""),
                 performances     = cbind(parameters, ams = model.ams, dispersion = model.variances),
                 best.model       = if (TRUE) {
                   modeltmp <- if (useFormula)
                     do.call(METHOD, c(list(train.x, data = data),
                                       pars, list(...)))
                   else
                     do.call(METHOD, c(list(x = train.x,
                                            y = train.y),
                                       pars, list(...)))
                   call[[1]] <- as.symbol("best.tune")
                   modeltmp$call <- call
                   modeltmp
                 }
  ),
  class = "tune"
  )
}

ams_tune_svm_orig <- function(METHOD, train.x, train.y = NULL, data = list(),
                         training_data_weights = NULL,
                         validation.x = NULL, validation.y = NULL,
                         ranges = NULL, predict.func = predict,
                         cross = 5,
                         ...
) {
  call <- match.call()
  
  ## internal helper functions
  resp <- function(formula, data) {
    
    model.response(model.frame(formula, data))
  }
  
  ## parameter handling
  validation.x <- validation.y <- NULL
  useFormula <- is.null(train.y)
  if (useFormula && (is.null(data) || length(data) == 0))
    data <- model.frame(train.x)
  if (is.vector(train.x)) train.x <- t(t(train.x))
  if (is.data.frame(train.y))
    train.y <- as.matrix(train.y)
  
  ## prepare training indices
  n <- nrow(if (useFormula) data else train.x)
  perm.ind <- sample(n)
  if (cross > n)
    stop(sQuote("cross"), " must not exceed sampling size!")
  if (cross == 1)
    stop(sQuote("cross"), " must be greater than 1!")
  train.ind <- tapply(1:n, cut(1:n, breaks = cross), function(x) perm.ind[-x])
  
  ## find best model
  parameters <- if (is.null(ranges))
    data.frame(dummyparameter = 0)
  else
    expand.grid(ranges)
  p <- nrow(parameters)
  model.variances <- model.ams <- c()
  
  ## - loop over all models
  for (para.set in 1:p) {
    print(paste("para.set:", para.set))
    sampling.ams <- c()
    
    ## - loop over all training samples
    for (sample in 1:length(train.ind)) {
      print(paste("sample:", sample))
      ## train one model
      pars <- if (is.null(ranges))
        NULL
      else
        lapply(parameters[para.set,,drop = FALSE], unlist)
      
      model <- if (useFormula)
        do.call(METHOD, c(list(train.x,
                               data = data,
                               subset = train.ind[[sample]]),
                          pars, list(...)
        )
        )
      else
        do.call(METHOD, c(list(train.x[train.ind[[sample]],],
                               y = train.y[train.ind[[sample]]]),
                          pars, list(...)
        )
        )
      
      ## predict validation set
      pred <- predict.func(model,
                           if (!is.null(validation.x))
                             validation.x
                           else if (useFormula)
                             data[-train.ind[[sample]],,drop = FALSE]
                           else if (inherits(train.x, "matrix.csr"))
                             train.x[-train.ind[[sample]],]
                           else
                             train.x[-train.ind[[sample]],,drop = FALSE]
      )
      
      ## compute performance measure
      true.y <- if (!is.null(validation.y))
        validation.y
      else if (useFormula) {
        if (!is.null(validation.x))
          resp(train.x, validation.x)
        else
          resp(train.x, data[-train.ind[[sample]],])
      } else
        train.y[-train.ind[[sample]]]
      
      if (is.null(true.y)) true.y <- rep(TRUE, length(pred))
      
      adj_weights <- adjust_weights(cbind(data, training_data_weights),
                                    cbind(data, training_data_weights)[train.ind[[sample]], ],
                                    unadjusted_weight_col = as.character(quote(training_data_weights)))
      
      sampling.ams[sample] <- approx_median_sig(predictions = pred,
                                                labels = true.y,
                                                weights = adj_weights,
                                                b_reg = 10)
    }
    model.ams[para.set] <- mean(sampling.ams)
    model.variances[para.set] <- sd(sampling.ams)
  }
  
  ## return results
  best <- which.max(model.ams)
  pars <- if (is.null(ranges))
    NULL
  else
    lapply(parameters[best,,drop = FALSE], unlist)
  structure(list(best.parameters  = parameters[best,,drop = FALSE],
                 best.performance = model.ams[best],
                 method           = if (!is.character(METHOD))
                   deparse(substitute(METHOD)) else METHOD,
                 nparcomb         = nrow(parameters),
                 train.ind        = train.ind,
                 sampling         = if (cross == n) "leave-one-out" else
                   paste(cross,"-fold cross validation", sep=""),
                 performances     = cbind(parameters, ams = model.ams, dispersion = model.variances),
                 best.model       = if (TRUE) {
                   modeltmp <- if (useFormula)
                     do.call(METHOD, c(list(train.x, data = data),
                                       pars, list(...)))
                   else
                     do.call(METHOD, c(list(x = train.x,
                                            y = train.y),
                                       pars, list(...)))
                   call[[1]] <- as.symbol("best.tune")
                   modeltmp$call <- call
                   modeltmp
                 }
  ),
  class = "tune"
  )
}

