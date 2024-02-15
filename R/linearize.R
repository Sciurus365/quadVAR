#' Linearize a quadVAR object to produce a network.
#'
#' A quadVAR object is nonlinear, which means that the relationship between variables are not the same across different values of the variables. This function linearizes a quadVAR object by specifying the values of the variables that the linearized model will be based on, to facilitate interpretation. The linearized model is then expressed in an adjacency matrix, which can be used to produce a network.
#'
#' @references The idea of this linearization function is inspired by Kroc, E., & Olvera Astivia, O. L. (2023). The case for the curve: Parametric regression with second- and third-order polynomial functions of predictors should be routine. Psychological Methods. https://doi.org/10.1037/met0000629
#'
#' @param model A quadVAR object.
#' @param value A numeric vector of length 1 or the same as the number of nodes, that specifies the values of the variables that the linearized model will be based on. If the length is 1, the same value will be used for all variables. The default value is `NULL`, in which case the value will be set to 0 in calculation, which means (if `value_standardized = TRUE`) the linearized model will be based on the mean values of all variables.
#' @param value_standardized A logical value that specifies whether the input value is standardized or not. If TRUE, the input value will be regarded as standardized value, i.e., mean + `value` * sd (e.g., 0 is the mean, 1 is mean + sd, ...). If FALSE, the input value will regarded as in the raw scale of the input data. If the raw dataset was already standardized, this parameter does not have an effect. The default value is `TRUE`.
#' @return A linear_quadVAR_network with the following elements:
#' \itemize{
#'  \item `adj_mat`: the adjacency matrix of the linearized network.
#'  \item `standardized_value`: the standardized value that the linearized model is based on.
#'  \item `actual_value`: the value in the raw scale of the input data.
#'  \item `model`: the input quadVAR object.
#'  \item `value_standardized`: the same as the input.
#'  }
#' @export
#'
linear_quadVAR_network <- function(model, value = NULL, value_standardized = TRUE) {
  if(is.null(value)) {
    cli::cli_inform(c("i" = "The {.pkg quadVAR} model, being {.strong nonlinear}, generates a network {.strong meaningful only for specific variable values}. If values are unspecified, the linearization/the plot defaults to 0 (i.e., the mean values of all variables if `value_standardized = TRUE`), but this is not a complete description of the model estimation and may not be meaningful in all cases."), .frequency = "regularly", .frequency_id = "plot.quadVAR")
    value <- 0
  }

  # check if the length of value is 1 or the same as the number of nodes, using rlang
  if(length(value) != 1 && length(value) != ncol(model$data)) {
    cli::cli_abort("The length of value must be 1 or the same as the number of variables. The length of value is {length(value)}, while the number of variables is {ncol(model$data)}.")
  }

  if(length(value) == 1) {
    value <- rep(value, ncol(model$data))
  }

  data_mean <- colMeans(model$data)
  data_sd <- apply(model$data, 2, stats::sd)

  if(value_standardized) {
    standardized_value <- value
    actual_value <- value * data_sd + data_mean
  } else {
    actual_value <- value
    standardized_value <- (value - data_mean) / data_sd
  }

  adj_mat <- get_adj_mat(model, actual_value)

  return(structure(list(adj_mat = adj_mat, standardized_value = standardized_value, actual_value = actual_value, model = model, value_standardized = value_standardized), class = "linear_quadVAR_network"))
}

#' Extract the adjacency matrix from a quadVAR object.
#'
#' @inheritParams linear_quadVAR_network
#' @param value The `actual_value` in the output of [linear_quadVAR_network()].
#' @return An adjacency matrix.
get_adj_mat <- function(model, value) {
  n_nodes <- ncol(model$data)
  adj_mat <- matrix(0, nrow = n_nodes, ncol = n_nodes)
  model_summ <- stats::coef(model, silent = TRUE)
  model_eqs <- quadVAR_to_dyn_eqns(model, minus_self = FALSE)
  names(value) <- paste0("X", 1:n_nodes)
  # now we have all the formula. we still need to calculate the partial derivatives for each variables.
  # the variable names are X1, X2, ... , Xn_nodes.
  # all partial derivatives are calculated at the value of the variables.
  # they together form the matrix we need.

  for(i in 1:n_nodes) {
    eq <- model_eqs[[i]]
    # return(eq)
    adj_mat[, i] <- stats::deriv(rlang::parse_expr(eq), namevec = paste0("X", 1:n_nodes)) %>% eval(envir = as.list(value)) %>% attr("gradient")
  }

  colnames(adj_mat) <- rownames(adj_mat) <- colnames(model$data)

  return(adj_mat)
}

#' Transform a quadVAR object to a list of dynamic equations.
#'
#' @param model A quadVAR object.
#' @param minus_self Whether to subtract the term itself from the equation. If `TRUE`, the equation will be in the form of (0 =) ` ... - X1`; if `FALSE`, the equation will be in the form of (X1 = )`...`.
#' @return A list of dynamic equations in characters. You can also use [rlang::parse_expr()] to parse them into expressions.
#' @export
quadVAR_to_dyn_eqns <- function(model, minus_self = TRUE) {
  n_nodes <- ncol(model$data)
  eqs <- vector("list", n_nodes)
  model_summary <- stats::coef(model, silent = TRUE)

  model_summary <- model_summary %>%
    # for the column `effect`, if there are multiple terms, which means they are in the form of X, a number, X, another number, like `X1X1`, `X1X2`,..., we need to insert `*` between the first number and the second X.
    dplyr::mutate(effect = stringr::str_replace_all(effect, "([0-9]+)(X)", "\\1*\\2")) %>%
    # remove terms with zero estimates.
    dplyr::filter(estimate != 0)

  for(j in 1:n_nodes){
    model_summary_j <- model_summary %>% dplyr::filter(model == j)
    df_terms <- model_summary_j %>% tidyr::unite("term", estimate, effect, sep = "*")
    eqs[[j]] <- paste0(df_terms$term, collapse = " + ")
    if(minus_self) eqs[[j]] <- paste0(eqs[[j]], " - X", j)
    if(eqs[[j]] == "") eqs[[j]] <- "0"
  }

  return(eqs)
}

#' @describeIn linear_quadVAR_network Produce a plot for the linearized quadVAR model. If `interactive = FALSE`, the output will be a qgraph object, which can be further used to calculate centrality measures using, e.g., [qgraph::centrality()] and [qgraph::centralityPlot()].
#' @param x A linear_quadVAR_network object.
#' @param interactive Whether to produce an interactive plot using `shiny` (in which the user can change the values of variables interactively) or a static plot using [qgraph::qgraph()]. Default is `FALSE`.
#' @param ... Other arguments passed to [qgraph::qgraph()].
#' @export
plot.linear_quadVAR_network <- function(x, interactive = FALSE, ...) {
  if(!interactive) {
    qgraph::qgraph(x$adj_mat, directed = TRUE, diag = TRUE, ...)
  } else {
    n_nodes <- ncol(x$model$data)
    original_qg <- plot(x, interactive = FALSE)
    original_layout <- original_qg$layout

    if(x$value_standardized) {
      range_mins <- rep(-3, n_nodes)
      range_maxs <- rep(3, n_nodes)
      var_means <- rep(0, n_nodes)
      steps <- rep(0.1, n_nodes)
    } else {
      var_means <- colMeans(x$model$data)
      var_sds <- apply(x$model$data, 2, stats::sd)
      range_mins <- var_means - 3 * var_sds
      range_maxs <- var_means + 3 * var_sds
      # steps: it should be the largest value that is 1*10^n, for which n is an integer, that smaller than 1/60 of the range.
      steps <- 10^floor(log10((range_maxs - range_mins) / 60))
      var_means <- round(var_means, -log10(steps))
      range_mins <- round(range_mins, -log10(steps))
      range_maxs <- round(range_maxs, -log10(steps))
    }

    sliders <- vector("list", n_nodes)
    for(i in 1:n_nodes) {
      sliders[[i]] <- shiny::sliderInput(paste0("slider_X", i), label = paste0("X", i), min = range_mins[i], max = range_maxs[i], value = var_means[i], step = steps[i])
    }
    ui <- shiny::fluidPage(
      theme = shinythemes::shinytheme("simplex"),
      shiny::navbarPage(
        "quadVAR Network",
        shiny::tabPanel(
          "Network",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              sliders
            ),
            shiny::mainPanel(
              shiny::plotOutput("network")
            )
          )
        ),
        shiny::tabPanel(
          "About",
          shiny::p("This is a Shiny app for the quadVAR network plot. You can change the values of variables in the sidebar to see how the network changes."),
          shiny::p("This app is generated by the `quadVAR` package. For more information, please visit https://github.com/Sciurus365/quadVAR.")
        )
      )
    )

    # Define server logic
    server <- function(input, output) {
      # Create the network graph
      output$network <- shiny::renderPlot({
        # Add the slider values to the data
        value <- lapply(paste0("X", 1:n_nodes), function(x) {
          input[[paste0("slider_", x)]]
        }) %>% unlist()
        # Plot the network graph
        linear_quadVAR_network(x$model, value = value, value_standardized = x$value_standardized) %>%
          plot(interactive = FALSE, layout = original_layout, ...)
      })
    }

    # Run the application
    shiny::shinyApp(ui = ui, server = server)
  }
}

