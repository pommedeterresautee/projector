#' Build Annoy model
#'
#' [RcppAnnoy] model is used to retrieve the most similar vectors to a pivot one.
#' This function builds the [RcppAnnoy] model.
#'
#' @param vectors [matrix] where each row is an observation. [rownames] should contain textual versions of the vectors.
#' @param number_trees [integer] counting the number of trees to grow in Annoy (for neighbor search). More gives better results but is slower to compute.
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
get_annoy_model <- function(vectors, number_trees) {
  assert_that(length(rownames(vectors)) > 0)
  annoy_model <- new(AnnoyAngular, ncol(vectors))
  for (i in seq(nrow(vectors))) {
    annoy_model$addItem(i - 1, vectors[i,])
  }
  annoy_model$build(number_trees)
  attr(annoy_model, "dict") <- rownames(vectors)
  annoy_model
}

#' Retrieve the most closest vector representation
#'
#' Use Annoy to rapidly retrieve the `n` most closest representation of a text.
#'
#' @param word [character] containing the pivot word
#' @param dict [character] containing all possible texts
#' @param annoy_model [RcppAnnoy] model
#' @param n number of elements to retrieve
#' @param search_k number of nodes to search in (Annoy parameter). Higher is better and slower.
#' @importFrom assertthat assert_that is.count is.string
#' @keywords internal
get_neighbors <- function(word, dict, annoy_model, n, search_k) {
  assert_that(is.string(word))
  assert_that(is.count(n))
  assert_that(is.count(search_k))
  assert_that(isTRUE(word %in% dict), msg = paste("Text not included in provided embeddings:", word))
  position <- which(word == dict)
  assert_that(is.count(position))
  l <- annoy_model$getNNsByItemList(position - 1, n, search_k, TRUE)
  l$item <- l$item + 1
  l$text <- dict[l$item]
  l
}

#' Compute 2D coordinates using PCA
#'
#' Rapid to compute but not always very meaningful.
#' Data are centered and scaled.
#'
#' @param vectors [matrix] containing the `n` closest neighbor
#' @param transformations transformations applied to vectors before computing PCA
#' @importFrom stats prcomp
#' @keywords internal
get_coordinates_pca <- function(vectors, transformations = c("center", "scaled"), ...) {
  scale <- "center" %in% transformations
  center <- "scaled" %in% transformations
  pca <- prcomp(vectors, center = center, scale. = scale)
  data.frame(pca$x[,1:2])
}

#' Compute 2D coordinates using `T-SNE`
#'
#' Better results but slow.
#' @param vectors [matrix] containing the `n` closest neighbor
#' @param max_iter maximum number of epoch (for `T-SNE` learning)
#' @param perplexity how to balance attention between local and global aspects of data
#' @param verbose print debug information (for `T-SNE` learning)
#' @importFrom Rtsne Rtsne
#' @keywords internal
get_coordinates_tsne <- function(vectors, max_iter = 500, perplexity = 30, verbose = FALSE, ...) {
  stop_lying_iter <- min(max_iter / 2, 250)
  tsne_model_1 <- Rtsne(vectors, check_duplicates = FALSE, pca = TRUE, max_iter = max_iter, perplexity = perplexity, theta = 0.5, dims = 2, verbose = verbose, stop_lying_iter = stop_lying_iter)
  data.frame(tsne_model_1$Y)
}

#' Compute 2D coordinates of vectors
#'
#' Transform original vectors in 2D coordinates applying either:
#' * [PCA](https://en.wikipedia.org/wiki/Principal_component_analysis)
#' * [T-SNE](https://distill.pub/2016/misread-tsne/).
#'
#' @param vectors [matrix] containing the `n` closest neighbor
#' @param projection_type [character] defining the algorithm to use to compute the coordinates. (`tsne` or `pca`)
#' @param ... parameters passed to projection algorithm (`max_iter`, `perplexity`, `verbose`, `transformations`)
#' @importFrom assertthat assert_that is.string
#' @keywords internal
get_coordinates <- function(vectors, projection_type, ...) {
  assert_that(is.matrix(vectors))
  assert_that(is.string(projection_type))
  assert_that(projection_type %in% c("pca", "tsne"))
  coordinates <- switch(projection_type,
              tsne = get_coordinates_tsne(vectors, ...),
              pca = get_coordinates_pca(vectors, ...))

  colnames(coordinates) <- c("x", "y")
  coordinates$text <- rownames(vectors)
  coordinates[c("text", "x", "y")]
}

#' Center coordinates around the pivot vector
#' @param coordinates [data.frame] containing coordinates to center. First position is the pivot.
#' @keywords internal
center_coordinates <- function(coordinates) {
  coordinates$x <- coordinates$x - coordinates[1,]$x
  coordinates$y <- coordinates$y - coordinates[1,]$y
  coordinates
}

#' Retrieve a list of neighbor vectors
#'
#' Use [RcppAnnoy] to rapidly retrieve a list of vector neighbors.
#'
#' Transform original vectors in 2D coordinates applying either:
#' * [PCA](https://en.wikipedia.org/wiki/Principal_component_analysis)
#' * [T-SNE](https://distill.pub/2016/misread-tsne/).
#'
#' @param text [character] containing the text related to the pivot vector
#' @param projection_type [character] defining the algorithm to use to compute the coordinates. (`tsne` or `pca`)
#' @param annoy_model [RcppAnnoy] model
#' @param n number of neighbors to retrieve
#' @param search_k number of nodes to search in ([RcppAnnoy] parameter). Higher = ++precision & --speed
#' @param center_pivot put pivot text in the middle of the graph
#' @param ... parameters passed to projection algorithm (`max_iter`, `perplexity`, `verbose`, `transformations`)
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
#'
#' selected_word <- "out"
#' df <- retrieve_neighbors(text = selected_word,
#'                          projection_type = "tsne",
#'                          annoy_model = annoy_model,
#'                          n = 1000)
#' }
#' @importFrom assertthat assert_that is.flag
#' @export
retrieve_neighbors <- function(text, projection_type, annoy_model, n, search_k = max(10000, 10 * n), center_pivot = TRUE, ...) {
    assert_that(is.flag(center_pivot))
    dict <- annoy_model@dict
    search_k <- min(length(dict), search_k)
    l <- get_neighbors(text, dict, annoy_model, n, search_k)
    vectors <- list()
    for (i in l$item) {
      vectors[[i]] <- annoy_model$getItemsVector(i)
    }
    mat <- do.call(rbind, vectors)
    rownames(mat) <- dict[l$item]
    df <- get_coordinates(mat, projection_type, ...)
    if (center_pivot) center_coordinates(df) else df
}

#' Plot vectors on a 2D plan
#'
#' Plot the text on an interactive scatter plot.
#'
#' @param coordinates [data.frame] containing 2D coordinates of texts.
#' @param min_cluster_size [integer] corresponding to the minimum size of a vector (for the colors).
#' @examples
#' if(interactive()){
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
#'
#' selected_word <- "out"
#' df <- retrieve_neighbors(text = selected_word,
#'                          projection_type = "tsne",
#'                          annoy_model = annoy_model,
#'                          n = 1000)
#' plot_texts(df, 3)
#' }
#' @importFrom dbscan hdbscan
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom plotly plot_ly layout
#' @importFrom assertthat assert_that is.count
#' @export
plot_texts <- function(coordinates, min_cluster_size = 5) {
  assert_that(is.count(min_cluster_size))
  selected_word <- coordinates$text[1]
  cl <- hdbscan(coordinates[c("x", "y")], minPts = min_cluster_size)
  number_cluster <- length(unique(cl$cluster))
  colors <- colorRampPalette(brewer.pal(min(11, number_cluster), "Paired"))(number_cluster)
  colors <- colors[cl$cluster + 1]

  p <- plot_ly(coordinates, x = ~x, y = ~y, name = "default", text = ~text, hoverinfo = "text", type = "scatter", mode = "markers", marker = list(size = ifelse(coordinates$text == selected_word, 30, 10), color = colors))

  remove_axis_info <- list(
    title = "",
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )

  p <- layout(p, xaxis = remove_axis_info, yaxis = remove_axis_info)
  p$elementId <- NULL # remove a stupid warning
  p
}

