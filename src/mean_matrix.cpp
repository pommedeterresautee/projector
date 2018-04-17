// [[Rcpp::plugins("cpp11")]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <map>

using namespace Rcpp;
using namespace RcppParallel;


class Projector{
public:

  Projector(const NumericMatrix& embeddings, bool na_if_unknwown_word_p) {


    nb_columns = embeddings.ncol();
    CharacterVector row_names_r = rownames(embeddings);
    na_vector = NumericVector(nb_columns, NumericVector::get_na());
    na_if_unknwown_word = na_if_unknwown_word_p;

    mat = std::vector<double>(embeddings.ncol() * embeddings.nrow() );

    for (R_xlen_t i = 0; i < embeddings.nrow(); ++i) {
      for (R_xlen_t j = 0; j < embeddings.ncol(); ++j) {
        mat[i * nb_columns + j] = embeddings(i, j);
      }
    }


    for (int i = 0; i < row_names_r.size(); ++i) {
      String row_name = row_names_r[i];
      map_row_names_position.insert(std::make_pair(row_name, i));
    }
  }

  NumericMatrix average_vectors(const CharacterVector& texts) {

    NumericMatrix result_mat(texts.size(), nb_columns);

    for (int i = 0; i < texts.size(); i++) {
      Rcpp::checkUserInterrupt();

      bool return_na = false;

      String selected_rows = texts[i];
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
        NumericVector row_mat = wrap(subset_matrix(index_match_rows));
        result_mat(i, _) = row_mat;
      }
    }

    return result_mat;
  }

private:
  std::vector<double> mat;
  NumericVector na_vector;
  std::map<std::string, int > map_row_names_position;
  bool na_if_unknwown_word;
  R_xlen_t nb_columns;

  std::vector<std::string> split_string(const std::string& text) const {
    std::vector<std::string> items;
    std::istringstream iss(text);
    copy(std::istream_iterator<std::string>(iss),
         std::istream_iterator<std::string>(),
         std::back_inserter(items));

    return items;
  }

  std::vector<double> subset_matrix(const std::vector<size_t>& index_match_rows) const {
    size_t nb_rows = index_match_rows.size();

    std::vector<double> result_subset_mat(nb_columns, 0);

    for (R_xlen_t j = 0; j < nb_columns; ++j) {
      for (size_t i = 0; i < nb_rows; ++i) {
        result_subset_mat[j] += mat[j + index_match_rows[i] * nb_columns];
      }
      result_subset_mat[j] /= nb_rows;
    }

    return result_subset_mat;
  }
};


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
//' @param texts [character] containing sentence made of words. Words are letters between separated by one or more spaces.
//' @param mat [matrix] where each row is a an embedding. Each row has a name and texts parameter are names of rows.
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
//' average_vectors("this function average vector", word_embeddings, TRUE)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix average_vectors(const CharacterVector& texts, const NumericMatrix& mat, bool na_if_unknwown_word) {

  Projector proj(mat, na_if_unknwown_word);

  return proj.average_vectors(texts);
}

