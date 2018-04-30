#' Build [RcppAnnoy] model
#'
#' [RcppAnnoy] model is used to retrieve the most similar vectors to a pivot one.
#' This function builds the [RcppAnnoy] model.
#'
#' @param vectors [matrix] where each row is an observation. [rownames] should contain textual versions of the vectors.
#' @param number_trees [integer] counting the number of trees to grow in [RcppAnnoy] (for neighbor search). More gives better results but is slower to compute.
#' @param verbose display progress of model building, plus some error message when a datapoint place is not found
#' @examples
#' if (interactive()){
#' # This example should be run with a higher quality model
#' # than the one embedded in fastrtext
#' library(projector)
#' library(fastrtext)
#'
#' model_test_path <- system.file("extdata",
#'                                "model_unsupervised_test.bin",
#'                                package = "fastrtext")
#' model <- load_model(model_test_path)
#' word_embeddings <- get_word_vectors(model,
#'                                     words = head(get_dictionary(model), 2e5))
#'
#' annoy_model <- get_annoy_model(word_embeddings, 5)
#' }
#' @importFrom RcppAnnoy AnnoyAngular
#' @importFrom assertthat assert_that
#' @import methods
#' @export
get_annoy_model <- function(vectors, number_trees, verbose = FALSE) {
  assert_that(length(rownames(vectors)) > 0)
  assert_that(is.matrix(vectors))
  annoy_model <- new(AnnoyAngular, ncol(vectors))
  annoy_model$setVerbose(verbose)
  
  for (i in seq(nrow(vectors))) {
    annoy_model$addItem(i - 1, vectors[i,])
  }
  annoy_model$build(number_trees)
  attr(annoy_model, "dict") <- rownames(vectors)
  
  # don't set the key now to not reorder the dict
  dict_position <- data.table("query" = annoy_model@dict)
  set(x = dict_position, j = "position", value = seq(annoy_model@dict) - 1)
  setkeyv(x = dict_position, cols = "query")
  attr(annoy_model, "dict_position") <- dict_position
  
  annoy_model
}

#' Retrieve the most closest vector representation of an indexed text
#'
#' Use [RcppAnnoy] to rapidly retrieve the `n` most closest representation of a text.
#' The text has to be already indexed in [RcppAnnoy]. 
#' If it is not the case, use [get_neighbors_from_free_text].
#'
#' @param text [character] containing the pivot text
#' @param annoy_model [RcppAnnoy] model
#' @param n number of elements to retrieve
#' @param search_k number of nodes to search in ([RcppAnnoy] parameter). Higher is better and slower.
#' @importFrom assertthat assert_that is.count is.string
#' @export
get_neighbors_from_text <- function(text, annoy_model, n, search_k) {
  assert_that(is.string(text))
  assert_that(is.count(n))
  assert_that(is.count(search_k) | search_k == -1)
  position <- get_word_position(text, annoy_model)
  assert_that(is.count(position) | position == 0, msg = paste("Text not included in provided embeddings:", text))
  l <- annoy_model$getNNsByItemList(position, n, search_k, TRUE)
  l$text <- annoy_model@dict[l$item + 1]
  l
}

#' Retrieve the most closest vector representation of a text provided itself as a vector
#'
#' Use [RcppAnnoy] to rapidly retrieve the `n` most closest representation of a text.
#'
#' @param vec [numeric] containing the pivot vector
#' @param annoy_model [RcppAnnoy] model
#' @param n number of elements to retrieve
#' @param search_k number of nodes to search in ([RcppAnnoy] parameter). Higher is better and slower.
#' @importFrom assertthat assert_that is.count
#' @export
get_neighbors_from_vector <- function(vec, annoy_model, n, search_k) {
  assert_that(is.numeric(vec))
  assert_that(is.count(n))
  assert_that(is.count(search_k) | search_k == -1)
  assert_that(length(annoy_model$getItemsVector(0)) == length(vec))
  
  l <- annoy_model$getNNsByVectorList(vec, n, search_k, TRUE)
  l$text <- annoy_model@dict[l$item + 1]
  l
}

#' Retrieve vector representation  of an indexed text
#'
#' Retrieve from [RcppAnnoy] the vector related to an indexed text.
#'
#' @param text [character] containing the text
#' @param annoy_model [RcppAnnoy] model
#' @importFrom assertthat assert_that is.string
#' @examples
#' if (interactive()){
#' library(projector)
#' library(fastrtext)
#'
#' model_test_path <- system.file("extdata",
#'                                "model_unsupervised_test.bin",
#'                                package = "fastrtext")
#' model <- load_model(model_test_path)
#' word_embeddings <- get_word_vectors(model,
#'                                     words = head(get_dictionary(model), 2e5))
#'
#' annoy_model <- get_annoy_model(word_embeddings, 5)
#' 
#' print(get_vector_from_text("the", annoy_model))
#' }
#' @export
get_vector_from_text <- function(text, annoy_model) {
  assert_that(is.string(text))
  position <- get_word_position(text, annoy_model)
  assert_that(is.count(position) | position == 0, msg = paste("Text not included in provided embeddings:", text))
  annoy_model$getItemsVector(position)
}

#' Retrieve the most closest vector representation of a (free) text
#'
#' Use [RcppAnnoy] to rapidly retrieve the `n` most closest representation of a text.
#'
#' @param text [character] containing the text searched.
#' @param annoy_model [RcppAnnoy] model
#' @param n top n elements to retrieve
#' @param search_k number of nodes to search in ([RcppAnnoy] parameter). Higher is better (and slower).
#' @param projector_instance an instance of projector tool generated by [get_projector_instance]
#' @importFrom assertthat assert_that is.count is.string
#' @export
get_neighbors_from_free_text <- function(text, annoy_model, n, search_k, projector_instance) {
  assert_that(is.string(text))
  assert_that(is.count(n))
  assert_that(is.count(search_k) | search_k == -1)
  
  query_embedding <- projector_instance$average_vectors(texts = text)
  assert_that(all(!is.na(query_embedding)))
  l <- annoy_model$getNNsByVectorList(query_embedding, n, -1, TRUE)
  l$text <- annoy_model@dict[l$item + 1]
  l
}

#' Get instance of projector for embedding averaging
#'
#' Generate an instance of projector tool dedicated to embedding averaging.
#' Not to be used with 
#'
#' @param word_embeddings_mat a [matrix] containing word embeddings.
#' @param na_if_unknwown_word [TRUE] to return [NA] if one of the word is unknown, [FALSE] to work with remaining known words
#' @importFrom assertthat assert_that is.flag
#' @keywords internal
get_projector_instance <- function(word_embeddings, na_if_unknwown_word) {
  assert_that(is.matrix(word_embeddings))
  assert_that(is.flag(na_if_unknwown_word))
  new(projector, mat = word_embeddings, na_if_unknwown_word = na_if_unknwown_word)
}

#' Get word position
#' @param word [character] words to get the position from
#' @param annoy_model [RcppAnnoy] model
#' @keywords internal
get_word_position <- function(word, annoy_model) {
  # TODO replace by %where%
  annoy_model@dict_position[word, position]
}
#' Save [RcppAnnoy] model
#'
#' Save the content of the model in two files:
#' * the [RcppAnnoy] model
#' * the dictionary ([character] containing texts)
#' @param annoy_model [RcppAnnoy] model
#' @param path_annoy path for the [RcppAnnoy] model
#' @param path_dictionary path for the dictionary ([character] containing texts)
#' @importFrom assertthat assert_that is.string
#' @export
save_annoy_model <- function(annoy_model, path_annoy, path_dictionary) {
  assert_that(is(annoy_model, "Rcpp_AnnoyAngular"))
  assert_that(annoy_model$getNItems() > 0)
  assert_that(is.string(path_annoy))
  assert_that(is.string(path_dictionary))
  annoy_model$save(path_annoy)
  dict <- attr(annoy_model, "dict")
  dict_position <- attr(annoy_model, "dict_position")
  number_dimensions <- length(annoy_model$getItemsVector(0))
  saveRDS(list(dict = dict, number_dimensions = number_dimensions, dict_position = dict_position), file = path_dictionary)
}

#' Load [RcppAnnoy] model
#'
#' Load the content of the model from two files:
#' * the [RcppAnnoy] model
#' * the dictionary ([character] containing texts)
#' @param path_annoy path to the [RcppAnnoy] model
#' @param path_dictionary path to the dictionary ([character] containing texts)
#' @importFrom assertthat assert_that is.string
#' @importFrom data.table data.table setkeyv set
#' @export
load_annoy_model <- function(path_annoy, path_dictionary) {
  assert_that(is.string(path_annoy))
  assert_that(is.string(path_dictionary))
  param <- readRDS(path_dictionary)
  annoy_model <- new(AnnoyAngular, param$number_dimensions)
  annoy_model$load(path_annoy)
  attr(annoy_model, "dict") <- param$dict
  assert_that(annoy_model$getNItems() == length(annoy_model@dict))
  attr(annoy_model, "dict_position") <- param$dict_position
  annoy_model
}

globalVariables(c("position"))
