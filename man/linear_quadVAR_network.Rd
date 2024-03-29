% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearize.R
\name{linear_quadVAR_network}
\alias{linear_quadVAR_network}
\alias{plot.linear_quadVAR_network}
\title{Linearize a quadVAR object to produce a network.}
\usage{
linear_quadVAR_network(model, value = NULL, value_standardized = TRUE)

\method{plot}{linear_quadVAR_network}(x, interactive = FALSE, ...)
}
\arguments{
\item{model}{A quadVAR object.}

\item{value}{A numeric vector of length 1 or the same as the number of nodes, that specifies the values of the variables that the linearized model will be based on. If the length is 1, the same value will be used for all variables. The default value is \code{NULL}, in which case the value will be set to 0 in calculation, which means (if \code{value_standardized = TRUE}) the linearized model will be based on the mean values of all variables.}

\item{value_standardized}{A logical value that specifies whether the input value is standardized or not. If TRUE, the input value will be regarded as standardized value, i.e., mean + \code{value} * sd (e.g., 0 is the mean, 1 is mean + sd, ...). If FALSE, the input value will regarded as in the raw scale of the input data. If the raw dataset was already standardized, this parameter does not have an effect. The default value is \code{TRUE}.}

\item{x}{A linear_quadVAR_network object.}

\item{interactive}{Whether to produce an interactive plot using \code{shiny} (in which the user can change the values of variables interactively) or a static plot using \code{\link[qgraph:qgraph]{qgraph::qgraph()}}. Default is \code{FALSE}.}

\item{...}{Other arguments passed to \code{\link[qgraph:qgraph]{qgraph::qgraph()}}.}
}
\value{
A linear_quadVAR_network with the following elements:
\itemize{
\item \code{adj_mat}: the adjacency matrix of the linearized network.
\item \code{standardized_value}: the standardized value that the linearized model is based on.
\item \code{actual_value}: the value in the raw scale of the input data.
\item \code{model}: the input quadVAR object.
\item \code{value_standardized}: the same as the input.
}
}
\description{
A quadVAR object is nonlinear, which means that the relationship between variables are not the same across different values of the variables. This function linearizes a quadVAR object by specifying the values of the variables that the linearized model will be based on, to facilitate interpretation. The linearized model is then expressed in an adjacency matrix, which can be used to produce a network.
}
\section{Methods (by generic)}{
\itemize{
\item \code{plot(linear_quadVAR_network)}: Produce a plot for the linearized quadVAR model. If \code{interactive = FALSE}, the output will be a qgraph object, which can be further used to calculate centrality measures using, e.g., \code{\link[qgraph:centrality]{qgraph::centrality()}} and \code{\link[qgraph:CentAndClusfuns]{qgraph::centralityPlot()}}.

}}
\references{
The idea of this linearization function is inspired by Kroc, E., & Olvera Astivia, O. L. (2023). The case for the curve: Parametric regression with second- and third-order polynomial functions of predictors should be routine. Psychological Methods. https://doi.org/10.1037/met0000629
}
