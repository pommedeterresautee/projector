% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API_annoy.R
\name{get_neighbors_from_vector}
\alias{get_neighbors_from_vector}
\title{Retrieve the most closest vector representation of a text provided itself as a vector}
\usage{
get_neighbors_from_vector(vec, annoy_model, n, search_k)
}
\arguments{
\item{vec}{\link{numeric} containing the pivot vector}

\item{annoy_model}{\link{RcppAnnoy} model}

\item{n}{number of elements to retrieve}

\item{search_k}{number of nodes to search in (\link{RcppAnnoy} parameter). Higher is better and slower.}
}
\description{
Use \link{RcppAnnoy} to rapidly retrieve the \code{n} most closest representation of a text.
}
