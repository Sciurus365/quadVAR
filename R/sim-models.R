#' Simulate a 4-emotion model
#'
#' This function simulates a 4-emotion model which is nonlinear, bistable, discrete, and (almost) centered to zero. Adapted from the model described by van de Leemput et al. (2014).
#' @references van de Leemput, I. A., Wichers, M., Cramer, A. O., Borsboom, D., Tuerlinckx, F., Kuppens, P., ... & Scheffer, M. (2014). Critical slowing down as early warning for the onset and termination of depression. Proceedings of the National Academy of Sciences, 111(1), 87-92.
#' @param time The number of time steps to simulate.
#' @param init A vector of initial values for the four variables. Default is c(1.36, 1.36, 4.89, 4.89), which is one of the stable states of the model.
#' @param sd The standard deviation of the noise.
#'
#' @return A matrix with the simulated data.
#'
#' @seealso [true_model_4_emo()], [compare_4_emo()], [quadVAR()]
#' @export
sim_4_emo <- function(time = 200, init = c(1.36, 1.36, 4.89, 4.89), sd = 1) {
  output <- matrix(nrow = time, ncol = 5)
  colnames(output) <- c("time", "x1", "x2", "x3", "x4")
  output[, 1] <- 1:time
  output[1, 2:5] <- init
  for (i in 2:time) {
    x1 <- output[i - 1, 2]
    x2 <- output[i - 1, 3]
    x3 <- output[i - 1, 4]
    x4 <- output[i - 1, 5]
    x1_1 <- 0.8 + 1.5 * x1 - 0.1 * x1^2 + 0.02 * x1 * x2 - 0.1 * x1 * x3 - 0.1 * x1 * x4 + stats::rnorm(1, sd = sd)
    x2_1 <- 0.8 + 1.5 * x2 - 0.1 * x2^2 + 0.02 * x1 * x2 - 0.1 * x2 * x3 - 0.1 * x2 * x4 + stats::rnorm(1, sd = sd)
    x3_1 <- 0.8 + 1.5 * x3 - 0.1 * x3^2 + 0.02 * x3 * x4 - 0.1 * x1 * x3 - 0.1 * x2 * x3 + stats::rnorm(1, sd = sd)
    x4_1 <- 0.8 + 1.5 * x4 - 0.1 * x4^2 + 0.02 * x3 * x4 - 0.1 * x1 * x4 - 0.1 * x2 * x4 + stats::rnorm(1, sd = sd)
    output[i, 2:5] <- c(x1_1, x2_1, x3_1, x4_1)
  }
  return(output)
}

#' True model for 4-emotion model
#'
#' This function generate the true model for the 4-emotion model. It can used to compare the estimated model with the true model, or to plot the true model.
#' @param ... Parameters passed to [sim_4_emo()]. See [sim_4_emo()] for details.
#'
#' @return A true_model_4_emo object.
#' @export
#'
#' @examples
#' coef(true_model_4_emo())
#' plot(true_model_4_emo())
#'
#' if (interactive()) {
#'   # This code will only run in an interactive session
#'   plot(true_model_4_emo(), interactive = TRUE)
#' }
true_model_4_emo <- function(...) {
  # Here we make a dataset with the means of 2.80, which is the neutral equilibrium point of the system, and sd of 1, so that in the illustrations, the standardized value would mean how far the value is from the equilibrium point.
  return(structure(list(data = data.frame(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1), x3 = c(-1, 0, 1), x4 = c(-1, 0, 1)) + 2.80, vars = c("x1", "x2", "x3", "x4")), class = c("true_model_4_emo", "quadVAR")))
}

#' @describeIn true_model_4_emo This function returns the coefficients for the 4-emotion model. It is also used in other functions to generate the linearized version of the true model and to make plots. It returns a list of coefficients for the 4-emotion model, in the same format as [coef.quadVAR()]
#'
#' @param object A true_model_4_emo object.
#' @param ... Not in use.
#' @export
#' @seealso [true_model_4_emo()], [compare_4_emo()], [quadVAR()]
coef.true_model_4_emo <- function(object, ...) {
  output <- data.frame(
    model = rep(1:4, each = 14),
    effect = rep(c("X1", "X2", "X3", "X4", "X1X1", "X1X2", "X1X3", "X1X4", "X2X2", "X2X3", "X2X4", "X3X3", "X3X4", "X4X4"), 4),
    estimate = rep(0, 56)
  )
  # then fill in the estimates
  for (i in 1:4) {
    # then fill in the true effects
    if (i == 1) {
      output$estimate[output$model == i & output$effect %in% c("X1")] <- c(1.5)
      output$estimate[output$model == i & output$effect %in% c("X1X1", "X1X2", "X1X3", "X1X4")] <- c(-0.1, 0.02, -0.1, -0.1)
    } else if (i == 2) {
      output$estimate[output$model == i & output$effect %in% c("X2")] <- c(1.5)
      output$estimate[output$model == i & output$effect %in% c("X1X2", "X2X2", "X2X3", "X2X4")] <- c(0.02, -0.1, -0.1, -0.1)
    } else if (i == 3) {
      output$estimate[output$model == i & output$effect %in% c("X3")] <- c(1.5)
      output$estimate[output$model == i & output$effect %in% c("X1X3", "X2X3", "X3X3", "X3X4")] <- c(-0.1, -0.1, -0.1, 0.02)
    } else if (i == 4) {
      output$estimate[output$model == i & output$effect %in% c("X4")] <- c(1.5)
      output$estimate[output$model == i & output$effect %in% c("X1X4", "X2X4", "X3X4", "X4X4")] <- c(-0.1, -0.1, 0.02, -0.1)
    }
  }
  # print the output with the precision of 0.01

  output
}

#' @describeIn true_model_4_emo This function prints out the true model for the 4-emotion model in the same format as [RAMP::RAMP()], to help users to compare the true model and the estimated model.
#' @param which Which model to print out. There are four models in total, corresponding to the four variables.
#' @param x A true_model_4_emo object.
#' @return NULL, but prints out the true model.
#' @export
print.true_model_4_emo <- function(x, which = NULL, ...) {
  if (is.null(which)) {
    for (i in 1:4) {
      cat("[[", i, "]]\n", sep = "")
      print(x, which = i)
    }
  } else if (which == 1) {
    cat("True main effects: 1\n")
    cat("Coefficients for main effects: 1.5\n")
    cat("True Interaction effects: X1X1 X1X2 X1X3 X1X4\n")
    cat("Coefficients for interaction effects: -0.1, 0.02, -0.1, -0.1\n")
  } else if (which == 2) {
    cat("True main effects: 2\n")
    cat("Coefficients for main effects: 1.5\n")
    cat("True Interaction effects: X1X2 X2X2 X2X3 X2X4\n")
    cat("Coefficients for interaction effects: 0.02, -0.1, -0.1, -0.1\n")
  } else if (which == 3) {
    cat("True main effects: 3\n")
    cat("Coefficients for main effects: 1.5\n")
    cat("True Interaction effects: X1X3 X2X3 X3X3 X3X4\n")
    cat("Coefficients for interaction effects: -0.1, -0.1, -0.1, 0.02\n")
  } else if (which == 4) {
    cat("True main effects: 4\n")
    cat("Coefficients for main effects: 1.5\n")
    cat("True Interaction effects: X1X4 X2X4 X3X4 X4X4\n")
    cat("Coefficients for interaction effects: -0.1, -0.1, 0.02, -0.1\n")
  }

  invisible(NULL)
}

#' Compare estimated model with true model for 4-emotion model
#'
#' This function compares the estimated model with the true model for the 4-emotion model. It prints out the estimated coefficients and the true coefficients for the main effects and interaction effects.
#'
#' @param model The estimated model, using data simulated from [sim_4_emo()], and model estimated using [quadVAR()].
#' @param silent Whether to print out the results.
#'
#' @return Silently return data frame with the estimated coefficients and the true coefficients for the main effects and interaction effects, while printing out the results rounded to two digits if `silent = FALSE`.
#' @export
compare_4_emo <- function(model, silent = FALSE) {
  # first create a data frame with the correct format
  output <- data.frame(
    model = rep(1:4, each = 14),
    effect = rep(c("X1", "X2", "X3", "X4", "X1X1", "X1X2", "X1X3", "X1X4", "X2X2", "X2X3", "X2X4", "X3X3", "X3X4", "X4X4"), 4),
    estimate = rep(0, 56),
    true = rep(0, 56)
  )
  # then fill in the estimates
  for (i in 1:4) {
    # first fill in the main effects
    output$estimate[output$model == i & output$effect %in% paste0("X", model$NVAR_model[[i]]$mainInd)] <- model$NVAR_model[[i]]$beta.m
    # then fill in the interaction effects
    for (j in 1:length(model$NVAR_model[[i]]$interInd)) {
      output$estimate[output$model == i & output$effect == model$NVAR_model[[i]]$interInd[j]] <- model$NVAR_model[[i]]$beta.i[j]
    }
    # then fill in the true effects
    if (i == 1) {
      output$true[output$model == i & output$effect %in% c("X1")] <- c(1.5)
      output$true[output$model == i & output$effect %in% c("X1X1", "X1X2", "X1X3", "X1X4")] <- c(-0.1, 0.02, -0.1, -0.1)
    } else if (i == 2) {
      output$true[output$model == i & output$effect %in% c("X2")] <- c(1.5)
      output$true[output$model == i & output$effect %in% c("X1X2", "X2X2", "X2X3", "X2X4")] <- c(0.02, -0.1, -0.1, -0.1)
    } else if (i == 3) {
      output$true[output$model == i & output$effect %in% c("X3")] <- c(1.5)
      output$true[output$model == i & output$effect %in% c("X1X3", "X2X3", "X3X3", "X3X4")] <- c(-0.1, -0.1, -0.1, 0.02)
    } else if (i == 4) {
      output$true[output$model == i & output$effect %in% c("X4")] <- c(1.5)
      output$true[output$model == i & output$effect %in% c("X1X4", "X2X4", "X3X4", "X4X4")] <- c(-0.1, -0.1, 0.02, -0.1)
    }
  }

  # print the output with the precision of 0.01

  output_toprint <- output
  output_toprint$estimate <- round(output_toprint$estimate, 2)
  output_toprint$true <- round(output_toprint$true, 2)

  if (!silent) print(output_toprint)

  invisible(output)
}
