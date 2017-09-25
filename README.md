![projectoR](https://github.com/pommedeterresautee/projector/raw/master/tools/logo.png) 

[![Build Status](https://travis-ci.org/pommedeterresautee/projector.svg?branch=master)](https://travis-ci.org/pommedeterresautee/projector)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/pommedeterresautee/projector?branch=master&svg=true)](https://ci.appveyor.com/project/pommedeterresautee/projector)
[![Coverage Status](https://img.shields.io/codecov/c/github/pommedeterresautee/projector/master.svg)](https://codecov.io/github/pommedeterresautee/projector?branch=master)
[![Follow](https://img.shields.io/twitter/follow/pommedeterre33.svg?style=social)](https://twitter.com/intent/follow?screen_name=pommedeterre33)

*Project dense vector representations of texts on a 2D plan to better understand neural models applied to NLP.*

![VizProjector1](https://github.com/pommedeterresautee/projector/raw/master/tools/viz1.gif) 

Introduction
------------

Since the famous [word2vec](https://en.wikipedia.org/wiki/Word2vec), embeddings are everywhere in [NLP](https://en.wikipedia.org/wiki/Natural_language_processing) (and other close areas like [IR](https://en.wikipedia.org/wiki/Information_retrieval)).  
The main idea behind embeddings is to represent texts (made of characters, words, sentences, or even larger blocks) as `numeric` vectors.  
This works very well and provides some abilities unreachable with the classic [BoW](https://en.wikipedia.org/wiki/Bag-of-words_model) approach.  
However, embeddings (e.g. vector representations) are difficult to understand, analyze (and debug) for humans because they are made of much more than just 3 dimensions.  

**One well known way to get a sense of understanding of embeddings is to to project them on a 2D scatter plot and visualize the distances between texts, search for clusters and so on...**  
This is the very purpose of this package!

2 algorithms can be used for projection:

* [PCA](https://en.wikipedia.org/wiki/Principal_component_analysis) (rapid)
* [T-SNE](https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding) (slow but better results)

An interactive module is also provided.

This package is partially inspired from [Tensorflow projector](http://projector.tensorflow.org/).

Installation
------------

You can install the `projector` package from Github as follows:

```R
# From Github
# install.packages("devtools")
devtools::install_github("pommedeterresautee/projector")
```

**WARNING** : This package depends of [RcppAnnoy](https://github.com/eddelbuettel/rcppannoy). There was a small bug in [RcppAnnoy](https://github.com/eddelbuettel/rcppannoy) package (version 0.0.9) which is now fixed only on Gihub but not yet on Cran.

Please install the last version of RcppAnnoy from Github:

```R
devtools::install_github("eddelbuettel/rcppannoy")
```

Demo code
---------

The demo below uses a model embedded in [fastrtext](https://github.com/pommedeterresautee/fastrtext) package for convenience.  
This model is of a very low quality because of package size constraint from Cran.
It is highly advised to use a model pretrained from Facebook on Wikipedia (size is of several Gb) available [there](https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md).

```R
library(projector)
library(fastrtext)

model_test_path <- system.file("extdata",
                               "model_unsupervised_test.bin",
                               package = "fastrtext")
model <- load_model(model_test_path)
# Viz below is from English Wikipedia fastText model
# model <- load_model("~/Downloads/wiki.en.bin")
word_embeddings <- get_word_vectors(model, words = head(get_dictionary(model), 5e5))

annoy_model <- get_annoy_model(word_embeddings, 5)

# pivot_word <- "friendship" # for Wikipedia viz
pivot_word <- "out"
df <- retrieve_neighbors(text = pivot_word, projection_type = "tsne", annoy_model = annoy_model, n = 500)
plot_text(coordinates = df, min_cluster_size = 3)
```

![VizProjector1](https://github.com/pommedeterresautee/projector/raw/master/tools/viz1.gif) 

Interactive exploration
-----------------------

The exploration of the embeddings is even more powerful when you can play with them and see how it reacts.  
For that purpose the interactive shiny application lets you declare a word as a pivot and discover who are the `n` closest neighbors.

```R
interactive_embedding_exploration(annoy_model)
```

![VizProjector2](https://github.com/pommedeterresautee/projector/raw/master/tools/viz2.gif) 
