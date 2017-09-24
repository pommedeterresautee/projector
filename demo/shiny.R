library(projector)
library(fastrtext)
set.seed(42)

model_test_path <- system.file("extdata",
                               "model_unsupervised_test.bin",
                               package = "fastrtext")
model <- load_model(model_test_path)
#model <- load_model("/home/geantvert/workspace/justice-data/ML/models/fasttext/wiki.fr.bin")
word_embeddings <- get_word_vectors(model, words = head(get_dictionary(model), 2e5))

annoy_model <- get_annoy_model(word_embeddings, 5)

interactive_embedding_exploration(annoy_model)
