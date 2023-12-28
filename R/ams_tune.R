# METHOD <- svm
# train.x <- Label ~ .
# train.y = NULL
# data <- higgs_training
# kernel = "radial"
# ranges = list(cost = c(0.5, 1, 2),
#               gamma = c(0.01, 1/30, 0.1))
# predict.func = predict
# sampling = "cross"
# cross = 5
# training_data_weights <- higgs_training %>% 
#   mutate(Weights = training_weights)
# 
# adjusted_weight_test <- adjust_weights(higgs_data_orig,
#                                        higgs_data_orig[index,][train.ind[[2]],])

ams_test <- ams_tune_svm(svm,
                         Label ~ .,
                         data = higgs_training,
                         kernel = "radial",
                         cross = 5,
                         training_data_weights = training_weights,
                         ranges = list(
                           # cost = c(0.5, 1, 2),
                           gamma = c(0.01, 0.1, 0.5)
                         ))
ams_test$best.parameters

ams_tune_svm <- function(METHOD, train.x, train.y = NULL, data = list(),
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
