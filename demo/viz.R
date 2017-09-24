library(projector)
library(fastrtext)
library(plotly)

model_test_path <- system.file("extdata",
                               "model_unsupervised_test.bin",
                               package = "fastrtext")
model <- load_model(model_test_path)
#model <- load_model("/home/geantvert/workspace/justice-data/ML/models/fasttext/wiki.fr.bin")
word_embeddings <- get_word_vectors(model, words = head(get_dictionary(model), 2e5))

annoy_model <- build_annoy_model(word_embeddings, 5)

selected_word <- "there"
b <- retrieve_neighbors(selected_word, word_embeddings, "tsne", annoy_model, 1000)
plot_text(b, 3)
