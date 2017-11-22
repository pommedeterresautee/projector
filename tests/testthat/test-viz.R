context("Word embeddings with fastrtext")

library(fastrtext)

model_test_path <- system.file("extdata",
                               "model_unsupervised_test.bin",
                               package = "fastrtext")

model <- load_model(model_test_path)
word_embeddings <- get_word_vectors(model, words = get_dictionary(model))
annoy_model <- get_annoy_model(word_embeddings, 5)

selected_word <- "there"

test_that("RcppAnnoy model properties", {
  expect_true(assertthat::has_attr(annoy_model, "dict"))
  expect_equal(annoy_model$getNItems(), length(annoy_model@dict))
  expect_gt(annoy_model$getNItems(), 0)
})

test_that("T-SNE", {
  number_neighbors <- 1e3
  b <- retrieve_neighbors(text = selected_word, projection_type = "tsne", annoy_model = annoy_model, n = number_neighbors)
  expect_length(b, 3)
  expect_equal(nrow(b), number_neighbors)
})

test_that("PCA - centered", {
  number_neighbors <- 1e3
  b <- retrieve_neighbors(text = selected_word, projection_type = "pca", annoy_model = annoy_model, n = number_neighbors, center_pivot = TRUE)
  expect_length(b, 3)
  expect_equal(nrow(b), number_neighbors)
  expect_equal(b[1,]$x, 0)
  expect_equal(b[1,]$y, 0)
})

test_that("PCA - not centered", {
  number_neighbors <- 1e3
  b <- retrieve_neighbors(text = selected_word, projection_type = "pca", annoy_model = annoy_model, n = number_neighbors, center_pivot = FALSE)
  expect_false(b[1,]$x == 0)
  expect_false(b[1,]$y == 0)
})

test_that("plot", {
  number_neighbors <- 1e3
  b <- retrieve_neighbors(text = selected_word, projection_type = "pca", annoy_model = annoy_model, n = number_neighbors)
  p <- plot_texts(b, 3)
  expect_equal(p$x$attrs[[1]]$type, "scatter")
  expect_equal(p$x$attrs[[1]]$mode, "markers")
  expect_length(p$x$attrs[[1]]$marker$size, number_neighbors)
  expect_length(p$x$attrs[[1]]$marker$color, number_neighbors)
})

test_that("save and load", {
  annoy_model_path <- tempfile()
  dict_path <- tempfile()
  save_annoy_model(annoy_model, annoy_model_path, dict_path)
  annoy_model_ter <- load_annoy_model(annoy_model_path, dict_path)
  for (i in seq(0, annoy_model$getNItems() - 1)) {
    expect_equal(annoy_model$getItemsVector(i), annoy_model_ter$getItemsVector(i))
  }
  expect_equal(annoy_model@dict, annoy_model_ter@dict)
})
