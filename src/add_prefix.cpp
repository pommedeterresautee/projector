// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::interfaces(r, cpp)]]

#include <Rcpp.h>
using namespace Rcpp;

static const std::string SPACE = " ";

std::string add_pr(const std::string& line, const std::string& prefix);

//' Add a prefix to each word
//'
//' Add a custom prefix to each word of a a line.
//' Code in C++ (efficient).
//'
//' @param texts a [character] containing the original text
//' @param prefix unit [character] containing the prefix to add (length == 1) or [character] with same length than texts
//' @return [character] with prefixed words.
//' @examples
//' add_prefix(c("this is a test", "this is another    test"), "#")
//' @export
// [[Rcpp::export]]
CharacterVector add_prefix(const CharacterVector& texts, CharacterVector prefix) {

  const bool unique_prefix = prefix.size() == 1;

  if (!unique_prefix && prefix.size() != texts.size()) {
    stop("prefix should be a single string or the same size than text");
  }

  std::string current_prefix;

  if (unique_prefix) {
    current_prefix = as<std::string>(prefix[0]);
  }

  CharacterVector result(texts.size());

  for (size_t i = 0; i < texts.size(); ++i) {
    if (!unique_prefix) {
      current_prefix = as<std::string>(prefix[i]);
    }
    result[i] = add_pr(as<std::string>(texts[i]), current_prefix);
  }
  return result;
}

inline std::string add_pr(const std::string& line, const std::string& prefix) {
  if (line.size() % 10 == 0) checkUserInterrupt();

  std::string result;
  result.reserve(line.size() * 2);

  bool last_char_is_space = true;
  bool current_char_is_space;
  for (char current_char: line) {
    current_char_is_space = std::isspace(current_char);
    if (last_char_is_space && !current_char_is_space) {
      result += prefix;
    }
    last_char_is_space = current_char_is_space;
    result += current_char;
  }
  return result;
}
