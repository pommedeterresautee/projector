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
    na_vector = NumericVector(nb_columns, NumericVector::get_na());
    na_if_unknwown_word = na_if_unknwown_word_p;

    word_embeddings = VectorD(embeddings.ncol() * embeddings.nrow());

    for (R_xlen_t i = 0; i < embeddings.nrow(); ++i) {
      for (R_xlen_t j = 0; j < embeddings.ncol(); ++j) {
        word_embeddings[i * nb_columns + j] = embeddings(i, j);
      }
    }

    CharacterVector row_names_r = rownames(embeddings);
    init_map(row_names_r);
  }

  projector(const CharacterVector& row_names_r) {
    if (row_names_r.length() == 0) {
      stop("empty row names");
    }
    na_if_unknwown_word = false;
    init_map(row_names_r);
  }

  NumericMatrix average_vectors(const CharacterVector& texts) {

    if (word_embeddings.size() == 0) {
      stop("Init embedding before using this instance");
    }

    if (texts.length() == 0) {
      stop("empty text vector");
    }

    NumericMatrix result_mat(texts.size(), nb_columns);

    for (R_xlen_t text_index = 0; text_index < texts.size(); text_index++) {
      if (text_index % 100 == 0) {
        checkUserInterrupt();
      }

      bool return_na = false;

      std::vector<std::string> words = split_string(static_cast<String>(texts[text_index]));
      std::vector<size_t> index_match_rows;
      search_position(words, index_match_rows, return_na, na_if_unknwown_word);

      if (return_na || index_match_rows.size() == 0) {
        result_mat(text_index, _) = na_vector;
      } else {
        NumericVector row_mat = wrap(subset_matrix(index_match_rows));
        result_mat(text_index, _) = row_mat;
      }
    }

    return result_mat;
  }

  IntegerVector get_position(const CharacterVector& original_texts) {
    if (original_texts.length() == 0) {
      stop("empty text vector");
    }

    std::vector<std::string> texts;
    std::transform(original_texts.begin(), original_texts.end(), std::back_inserter(texts), [](const String& text) { return text;});

    bool return_na = false;
    std::vector<size_t> index_match_rows;
    search_position(texts, index_match_rows, return_na, false);
    std::transform(index_match_rows.begin(),
                   index_match_rows.end(),
                   index_match_rows.begin(),
                   [](int i) { return i + 1;});

    return wrap(index_match_rows);
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

  void search_position(const std::vector<std::string>& searched_words, std::vector<size_t>& index_match_rows, bool& return_na, bool break_if_unknwown_word) {
    std::map<std::string, int >::iterator p;
    for (size_t word_index = 0; word_index < searched_words.size(); ++word_index) {
      p = map_row_names_position.find(searched_words[word_index]);
      if(p != map_row_names_position.end()) {
        index_match_rows.push_back(p->second);
      } else {
        if (break_if_unknwown_word) {
          return_na = true;
          break;
        }
      }
    }
  }

  void init_map(const CharacterVector& row_names_r) {
    for (int row_index = 0; row_index < row_names_r.size(); ++row_index) {
      map_row_names_position.insert(std::make_pair(static_cast<String>(row_names_r[row_index]), row_index));
    }
  }
};

RCPP_MODULE(PROJECTOR_MODULE) {
  class_<projector>("projector")
  .constructor<NumericMatrix, bool>("Tools related to vectors")
  .constructor<CharacterVector>("Tools related to vectors")
  .method("average_vectors", &projector::average_vectors, "Average vectors")
  .method("set_unknown_word", &projector::set_unknown_word, "Change behaviour when the word is unknown")
  .method("get_position", &projector::get_position, "get word position in the character vector");
}
