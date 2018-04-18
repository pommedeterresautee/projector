loadModule(module = "PROJECTOR_MODULE", what = TRUE)

#' @name projector
#' @useDynLib projector
#' @importFrom Rcpp loadModule cpp_object_initializer
#' @import methods
"_PACKAGE"

#' Rcpp_projector class
#'
#' @name Rcpp_projector-class
#'
#' @description
#' Efficient implementation of a function to average embeddings of words from a sentence.
#'
#' Constructor takes a [matrix] (`embeddings`) where each row is a word embedding (each row should have a name, the name being the word related to the embedding) and a [logical] (`na_if_unknwown_word_p`) set to [TRUE] if it is expected to fulfill a row with [NA] when at the minimum one word of the sentence is unknown, and set to [FALSE] where it is expected to only average vectors related to known words. If no word are known, the row will always be fulfilled by [NA], whatever the parameters are.
#'
#' Words are defined as letters between separated by one or more spaces in a sentence.
#'
#' @slot average_vectors provide [character] (`texts`) and return a [matrix] of sentence embeddings.
#' @slot set_unknown_word change value of parameter `na_if_unknwown_word_p` (see above).
NULL
