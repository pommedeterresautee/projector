#' Build Annoy model
#'
#' Annoy is used to retrieve the most similar vectors to a pivot one.
#' This function builds the Annoy model.
#' @param vectors [matrix] where each row is an observation
#' @param number_trees [integer] counting the number of trees to grow in Annoy (for neighboor search). More gives better results but is slower to compute.
#' @importFrom RcppAnnoy AnnoyAngular
#' @import methods
#' @export
build_annoy_model <- function(vectors, number_trees) {
  model <- new(AnnoyAngular, ncol(vectors))
  for (i in seq(nrow(vectors))) {
    model$addItem(i - 1, vectors[i,])
  }
  model$build(number_trees)
  model
}

#' Retrieve the most closest vector representation
#'
#' Use Annoy to rapidly retrieve the `n` most closest representation of a text.
#' @param word [character] containing the pivot word
#' @param dict [character] containing all possible texts
#' @param annoy_model Annoy model
#' @param n number of elements to retrieve
#' @param search_k number of nodes to search in (Annoy parameter). Higher is better and slower.
#' @importFrom assertthat assert_that is.count
get_neighbors <- function(word, dict, annoy_model, n, search_k) {
  assert_that(isTRUE(word %in% dict))
  position <- which(word == dict)
  assert_that(is.count(position))
  print(position)
  l <- annoy_model$getNNsByItemList(position - 1, n, search_k, TRUE)
  l$item <- l$item + 1
  l$text <- dict[l$item]
  l
}

#' Compute 2D coordinates using PCA
#'
#' Rapid to compute but not always very meaningful.
#'
#' @param vectors [matrix] containing the `n` closest neighboor
#' @importFrom stats prcomp
get_coordinates_pca <- function(vectors) {
  pca <- prcomp(vectors, center = TRUE, scale. = TRUE)
  data.frame(pca$x[,1:2])
}

#' Compute 2D coordinates using T-SNE
#'
#' Better results but slow.
#'
#' @param vectors [matrix] containing the `n` closest neighboor
#' @param max_iter maximum number of epoch (for T-SNE learning)
#' @param verbose print debug information
#' @importFrom Rtsne Rtsne
get_coordinates_tsne <- function(vectors, max_iter = 500, verbose = TRUE) {
  stop_lying_iter <- min(max_iter / 2, 250)
  tsne_model_1 <- Rtsne(vectors, check_duplicates = FALSE, pca = TRUE, perplexity = 30, theta = 0.5, dims = 2, verbose = verbose, max_iter = max_iter, stop_lying_iter = stop_lying_iter)
  data.frame(tsne_model_1$Y)
}

#' Compute 2D coordinates of vectors
#'
#' Plot function
#'
#' @param vectors [matrix] containing the `n` closest neighboor
#' @param projection_type [character] defining the algorithm to use to compute the coordinates. `tsne` or `pca`
#' @export
get_coordinates <- function(vectors, projection_type) {
  coordinates <- switch(projection_type,
              tsne = get_coordinates_tsne(vectors = vectors),
              pca = get_coordinates_pca(vectors = vectors))

  colnames(coordinates) <- c("x", "y")
  coordinates$text <- rownames(vectors)
  coordinates
}

#' Center coordinates around the pivot vector
#'
#' @param coordinates [data.frame] containing coordinates to center. First position is the pivot.
center_coordinates <- function(coordinates) {
  coordinates$x <- coordinates$x - coordinates[1,]$x
  coordinates$y <- coordinates$y - coordinates[1,]$y
  coordinates
}

#' Retrieve a list of neighbor vectors
#'
#' Use [RcppAnnoy]
#'
#' @param text [character] containing the text related to the pivot vector
#' @param embeddings [matrix] containing all possible vectors
#' @param projection_type [character] defining the algorithm to use to compute the coordinates
#' @param model [RcppAnnoy] model
#' @param n number of elements to retrieve
#' @param search_k number of nodes to search in (Annoy parameter). Higher is better and slower.
#' @export
retrieve_neighbors <- function(text, embeddings, projection_type, model, n, search_k = min(max(10000, 10 * n), nrow(embeddings))) {
    l <- get_neighbors(text, rownames(embeddings), model, n, search_k)
    df <- get_coordinates(embeddings[l$item,], projection_type)
    center_coordinates(df)
}

#' Plot vectors on a 2D plan
#'
#' Plot the text on an interactive scatter plot.
#'
#' @param coordinates [data.frame] containing 2D coordinates of texts.
#' @param min_cluster_size [integer] corresponding to the minimum size of a vector (for the colors).
#' @importFrom dbscan hdbscan
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom plotly plot_ly
#' @export
plot_text <- function(coordinates, min_cluster_size = 5) {
  selected_word <- coordinates$text[1]
  cl <- hdbscan(coordinates[, 1:2], minPts = min_cluster_size)
  number_cluster <- length(unique(cl$cluster))
  colors <- colorRampPalette(brewer.pal(min(11, number_cluster), "Paired"))(number_cluster)
  coordinates$colors <- colors[cl$cluster + 1]
  plot_ly(coordinates, x = ~x, y = ~y, name = "default", text = ~text, type = "scatter", mode = "markers", marker = list(size = ifelse(coordinates$text == selected_word, 30, 10), color = ~colors))
}

