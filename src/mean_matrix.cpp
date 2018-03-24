// [[Rcpp::plugins("cpp11")]]
#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix subset_matrix(const NumericMatrix& mat, const IntegerVector& index_match_rows) {
  R_xlen_t nb_rows = index_match_rows.length();
  R_xlen_t nb_columns = mat.ncol();

  NumericMatrix result_mat = no_init(nb_rows, nb_columns);

  for (R_xlen_t j = 0; j < nb_columns; j++) {
    for (R_xlen_t i = 0; i < nb_rows; i++) {
      result_mat(i, j) = mat(index_match_rows[i] - 1, j);
    }
  }

  return result_mat;
}

NumericVector col_means(const NumericMatrix& mat) {
  int nb_columns = mat.ncol();
  NumericVector out = no_init(nb_columns);

  for(int j=0; j < nb_columns; j++ ) {
    out[j] = mean(mat(_, j));
  }
  return out;
}

//' Average vectors
//'
//' Efficient implementation of a function to average vectors from a matrix.
//'
//' @param keys [list] containing ids of embeddings in the matrix. Each slot of the [list] is related to a sequence.
//' @param mat embedding [matrix]
//' @examples
//' if (interactive()){
//' # This example should be run with a higher quality model
//' # than the one embedded in fastrtext
//' library(projector)
//' library(fastrtext)
//'
//' model_test_path <- system.file("extdata",
//'                                "model_unsupervised_test.bin",
//'                                package = "fastrtext")
//' model <- load_model(model_test_path)
//' word_embeddings <- get_word_vectors(model,
//'                                     words = head(get_dictionary(model), 2e5))
//'
//' average_vectors(strsplit(x = "this function average vector", split = " "), word_embeddings)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix average_vectors(const List& keys, const NumericMatrix& mat) {
  NumericMatrix result_mat = no_init(keys.size(), mat.ncol());

  const NumericVector na_vector(mat.ncol(), NumericVector::get_na());
  const CharacterVector row_names = rownames(mat);
  const int na_value = NumericVector::get_na();
  CharacterVector selected_rows;
  IntegerVector index_match_rows;

  for (int i = 0; i < keys.size(); i++) {
    Rcpp::checkUserInterrupt();
    selected_rows = keys[i];
    index_match_rows = match(selected_rows, row_names);

    bool has_na = false;

    for (int j = 0; j < index_match_rows.size(); ++j) {
      if(index_match_rows[j] == na_value) {
        has_na = true;
        break;
      }
    }

    if (has_na) {
      result_mat(i, _) = na_vector;
    } else {
      NumericMatrix subset_mat = subset_matrix(mat, index_match_rows);
      result_mat(i, _) = col_means(subset_mat);
    }
  }

  return result_mat;
}

