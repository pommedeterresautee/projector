// [[Rcpp::plugins("cpp11")]]

#include <Rcpp.h>
#include <unordered_map>

using namespace Rcpp;
typedef std::vector<double> VectorD;

//' @export projector
class projector{
public:

  projector(const NumericMatrix& embeddings, bool na_if_unknwown_word_p) {

    if (embeddings.length() == 0) {
      stop("empty embedding");
    }

    nb_columns = embeddings.ncol();
    CharacterVector row_names_r = rownames(embeddings);
    na_vector = NumericVector(nb_columns, NumericVector::get_na());
    na_if_unknwown_word = na_if_unknwown_word_p;

    word_embeddings = VectorD(embeddings.ncol() * embeddings.nrow());

    for (R_xlen_t i = 0; i < embeddings.nrow(); ++i) {
      for (R_xlen_t j = 0; j < embeddings.ncol(); ++j) {
        word_embeddings[i * nb_columns + j] = embeddings(i, j);
      }
    }

    for (int row_index = 0; row_index < row_names_r.size(); ++row_index) {
      String row_name = row_names_r[row_index];
      map_row_names_position.insert(std::make_pair(row_name, row_index));
    }
  }

  NumericMatrix average_vectors(const CharacterVector& texts) {

    if (texts.length() == 0) {
      stop("empty text vector");
    }

    NumericMatrix result_mat(texts.size(), nb_columns);

    for (int text_index = 0; text_index < texts.size(); text_index++) {
      if (text_index % 100 == 0) {
        checkUserInterrupt();
      }

      bool return_na = false;

      String selected_rows = texts[text_index];
      std::vector<std::string> words = split_string(selected_rows);

      std::vector<size_t> index_match_rows;
      std::map<std::string, int >::iterator p;
      for (int word_index = 0; word_index < words.size(); ++word_index) {
        p = map_row_names_position.find(words[word_index]);
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
        result_mat(text_index, _) = na_vector;
      } else {
        NumericVector row_mat = wrap(subset_matrix(index_match_rows));
        result_mat(text_index, _) = row_mat;
      }
    }

    return result_mat;
  }

  void set_unknown_word(bool na_if_unknwown_word_p) {
    na_if_unknwown_word = na_if_unknwown_word_p;
  }

private:
  VectorD word_embeddings;
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

  VectorD subset_matrix(const std::vector<size_t>& index_match_rows) const {
    size_t nb_rows = index_match_rows.size();

    VectorD result_subset_mat(nb_columns, 0);

    for (R_xlen_t col_index = 0; col_index < nb_columns; ++col_index) {
      for (size_t row_index = 0; row_index < nb_rows; ++row_index) {
        result_subset_mat[col_index] += word_embeddings[col_index + index_match_rows[row_index] * nb_columns];
      }
      result_subset_mat[col_index] /= nb_rows;
    }

    return result_subset_mat;
  }
};

RCPP_MODULE(PROJECTOR_MODULE) {
  class_<projector>("projector")
  .constructor<NumericMatrix, bool>("Tools related to vectors")
  .method("average_vectors", &projector::average_vectors, "Average vectors")
  .method("set_unknown_word", &projector::set_unknown_word, "Change behaviour when the word is unknown");
}
