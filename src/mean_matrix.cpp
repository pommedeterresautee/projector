// [[Rcpp::plugins("cpp11")]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <map>

using namespace Rcpp;
using namespace RcppParallel;

inline std::vector<std::string> split_string(const std::string& text){
  std::vector<std::string> items;
  std::istringstream iss(text);
  copy(std::istream_iterator<std::string>(iss),
       std::istream_iterator<std::string>(),
       std::back_inserter(items));

  return items;
}

std::vector<double> subset_matrix(const NumericMatrix& mat, const std::vector<size_t>& index_match_rows) {
  size_t nb_rows = index_match_rows.size();
  R_xlen_t nb_columns = mat.ncol();

  std::vector<double> result_subset_mat(nb_columns, 0);

  for (R_xlen_t j = 0; j < nb_columns; ++j) {
    for (size_t i = 0; i < nb_rows; ++i) {
      result_subset_mat[j] += mat(index_match_rows[i], j);
    }
    result_subset_mat[j] /= nb_rows;
  }

  return result_subset_mat;
}

// struct WordEmbedding : public Worker {
//   // source matrix
//   const RMatrix<double> input;
//
//   RMatrix<double> word_embeding;
//
//   // destination matrix
//   RMatrix<double> output;
//
//   // initialize with source and destination
//   WordEmbedding(const NumericMatrix& input,
//                 const List& keys,
//                 const NumericMatrix& word_embedding,
//                 NumericMatrix output)
//     : input(input), word_embeding(word_embedding), output(output) {}
//
//   // take the square root of the range of elements requested
//   void operator()(std::size_t begin, std::size_t end) {
//     // std::transform(input.begin() + begin,
//     //                input.begin() + end,
//     //                output.begin() + begin,
//     //                ::sqrt);
//   }
// };

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
NumericMatrix average_vectors(const CharacterVector& keys, const NumericMatrix& mat, bool na_if_unknwown_word) {

  NumericMatrix result_mat(keys.size(), mat.ncol());

  const NumericVector na_vector(mat.ncol(), NumericVector::get_na());
  const CharacterVector row_names_r = rownames(mat);

  std::map<std::string, int > map_row_names_position;
  for (int i = 0; i < row_names_r.size(); ++i) {
    String row_name = row_names_r[i];
    map_row_names_position.insert(std::make_pair(row_name, i));
  }

  for (int i = 0; i < keys.size(); i++) {
    Rcpp::checkUserInterrupt();

    bool return_na = false;

    String selected_rows = keys[i];
    std::vector<std::string> words = split_string(selected_rows);

    std::vector<size_t> index_match_rows;
    std::map<std::string, int >::iterator p;
    for (int i = 0; i < words.size(); ++i) {
      p = map_row_names_position.find(words[i]);
      if(p != map_row_names_position.end()) {
        index_match_rows.push_back(p->second);
      } else {
        if (na_if_unknwown_word) {
          return_na = true;
          break;
        }
      }
    }

    if (return_na || index_match_rows.size() == 0) {
      result_mat(i, _) = na_vector;
    } else {
      NumericVector row_mat = wrap(subset_matrix(mat, index_match_rows));
      result_mat(i, _) = row_mat;
    }
  }

  return result_mat;
}

