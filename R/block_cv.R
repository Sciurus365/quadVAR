divide_vector <- function(n, block) {
  # Calculate the length of each block
  block_length <- floor(n / block)

  # Calculate the number of blocks with the same length
  full_blocks <- block - (n %% block)

  # Initialize the result vector
  result <- vector("list", block)

  # Distribute elements into blocks
  start_index <- 1
  for (i in 1:block) {
    end_index <- start_index + block_length - 1
    if (i <= (n %% block)) {
      end_index <- end_index + 1
    }
    result[[i]] <- start_index:end_index
    start_index <- end_index + 1
  }

  return(result)
}


# use block cv, since we are dealing with time series
block_cv_split <- function(data, block = 10) {
  n <- nrow(data)

  # note that n may not be divisible by block

  # divide indices into blocks
  blocks <- divide_vector(n, block)

  # create a list to store the indices
  indices <- vector("list", length = length(blocks))

  # loop over each block
  for (i in 1:length(blocks)) {
    # get the test indices
    test <- blocks[[i]]

    # get the train indices
    train <- unlist(blocks[-i])

    # store the indices
    indices[[i]] <- list(test = test, train = train)
  }

  # return the indices
  return(indices)
}

#' Use Block Cross-Validation to Evaluate Models
#'
#' This function uses block cross-validation to evaluate a model. The data is split into blocks, and the model is fit on all but one block and evaluated on the remaining block. This process is repeated for each block, and the mean squared error is calculated for each model.
#'
#' @param data A data frame.
#' @param dayvar A character string. The name of the variable that represents the day. This is required because this function use dayvar to specify the time point before the test block should not be used to predict the time point after the test block. If dayvar is not specified, then the function will create a new variable called "dayvar", which is a constant, and use it as the dayvar.
#' @param model A function. The model to be evaluated. The function should take a data frame as its first argument and return a `quadVAR` object. It can be, for example, `function(x) quadVAR(x, vars = c("var1", "var2"))`
#' @param block An integer. The number of blocks to use in the cross-validation. The default is 10.
#' @return A list of mean squared errors for each model.
#' @export
block_cv <- function(data, dayvar = NULL, model, block = 10) {
  # split the data
  indices <- block_cv_split(data, block)

  # create a list to store the predictions
  preds <- vector("list", length = length(indices))

  if (is.null(dayvar)) {
    data$dayvar <- 1
    dayvar <- "dayvar"
  }

  # loop over each fold
  for (i in 1:length(indices)) {
    # as we are doing time series modeling, we don't want to use the time point before the testing set to predict the time point after the testing set for training
    # also be aware of the first and the last block.
    # so what we should do is that, if the test set is not the first nor the last block, then we increase data$dayvar for the part AFTER the testing set by 1, so that in model construction, we don't use the time point before the testing set to predict the time point after the testing set for training

    if (i != 1 & i != length(indices)) {
      indices_train_after_test <- indices[[i]]$train
      indices_train_after_test <- indices_train_after_test[indices_train_after_test > max(indices[[i]]$test)]
      data[indices_train_after_test, dayvar] <- data[indices_train_after_test, dayvar] + 1
    }


    # fit the model on the training set
    fit <- model(data[indices[[i]]$train, ])
    # make predictions on the testing set
    preds[[i]] <- stats::predict(fit, newdata = data[indices[[i]]$test, ])
  }

  # return(preds)

  # calculate MSE
  # return(list(data[,fit$vars], do.call(rbind, preds)))
  mse <- lapply(c("VAR", "VAR_full", "quadVAR", "quadVAR_full", "AR"), function(model_type) mean((as.matrix(data[, fit$vars]) - do.call(rbind, lapply(preds, function(x) x[[model_type]])))^2, na.rm = TRUE))

  names(mse) <- c("VAR", "VAR_full", "quadVAR", "quadVAR_full", "AR")

  # return the MSE
  return(mse)
}
