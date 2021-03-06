% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API_annoy.R
\name{get_projector_instance}
\alias{get_projector_instance}
\title{Get instance of projector for embedding averaging}
\usage{
get_projector_instance(word_embeddings, na_if_unknwown_word)
}
\arguments{
\item{na_if_unknwown_word}{\link{TRUE} to return \link{NA} if one of the word is unknown, \link{FALSE} to work with remaining known words}

\item{word_embeddings_mat}{a \link{matrix} containing word embeddings.}
}
\description{
Generate an instance of projector tool dedicated to embedding averaging.
Not to be used with
}
\keyword{internal}
