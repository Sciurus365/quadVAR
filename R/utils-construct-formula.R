construct_formula <- function(vars, expressions) {
  rhs <- expressions %>%
    lapply(rlang::expr_deparse) %>%
    c() %>%
    paste0(collapse = "+") %>%
    rlang::parse_expr()

  lapply(
    vars,
    function(x) {
      x <- rlang::parse_expr(x)
      rlang::expr(!!x ~ !!rhs)
    }
  )
  # return a list of global models for each variable
}

construct_formula_AR <- function(vars, expressions) {
  result <- vector("list", length(vars))
  for (i in 1:length(vars)) {
    result[[i]] <- rlang::expr(!!(rlang::parse_expr(vars[i])) ~ !!(expressions[[i]]))
  }
  return(result)
}
