// [[Rcpp::plugins("cpp11")]]
#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix subset_matrix(const NumericMatrix& mat, const std::vector<double>& index_match_rows) {
  R_xlen_t nb_rows = index_match_rows.size();
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
//' Efficient implementation of a function to average embeddings stored in a [matrix].
//'
//' @param keys [list] containing ids of embeddings in the matrix. Each slot of the [list] is related to a sequence.
//' @param mat [matrix] where each row is a an embedding. Each row has a name and keys parameter are names of rows.
//' @param na_if_unknwown_word [TRUE] to fulfill a row with [NA] if one word of the document is unknown, and [FALSE] to only average known vectors
//' @return a [matrix] of embeddings where each row is related to each slot of the list. When an Id is not found, the full vector related to the sequence is [NA].
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
//' average_vectors(strsplit(x = "this function average vector", split = " "), word_embeddings, TRUE)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix average_vectors(const List& keys, const NumericMatrix& mat, bool na_if_unknwown_word) {
  NumericMatrix result_mat = no_init(keys.size(), mat.ncol());

  const NumericVector na_vector(mat.ncol(), NumericVector::get_na());
  const CharacterVector row_names = rownames(mat);
  CharacterVector selected_rows;
  std::vector<double> index_match_rows;

  for (int i = 0; i < keys.size(); i++) {
    Rcpp::checkUserInterrupt();
    selected_rows = keys[i];
    index_match_rows = Rcpp::as<std::vector<double> >(match(selected_rows, row_names));
    bool has_na = false;

    for (int j = 0; j < index_match_rows.size(); ++j) {
      if(std::isnan(index_match_rows[j])) {
        if (na_if_unknwown_word) {
          has_na = true;
          break;
        } else {
          index_match_rows.erase(index_match_rows.begin() + j);
        }
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

