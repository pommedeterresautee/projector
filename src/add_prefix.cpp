#include <Rcpp.h>
using namespace Rcpp;

static const std::string SPACE = " ";

std::string add_pr(const std::string& line, const std::string& prefix);

//' Add a prefix to each word
//'
//' Add a custom prefix to each word of a a line.
//' Apply it even if the precedent the word is preceded by a punctuation.
//' Code in C++ (efficient).
//'
//' @param texts a [character] containing the original text
//' @param prefix a unit [character] containing the prefix to add (length == 1)
//' @return [character] with prefixed words.
//' @examples
//' add_prefix(c("this is a test", "this is another    test"), "#")
//' @export
// [[Rcpp::export]]
CharacterVector add_prefix(const CharacterVector& texts, CharacterVector prefix) {

  if (prefix.size() != 1) {
    stop("prefix should be a single string");
  }

  const std::string prefixS = as<std::string>(prefix[0]);
  CharacterVector result(texts.size());

  std::transform(texts.begin(),
                 texts.end(),
                 result.begin(),
                 [&prefixS](String line) { return add_pr(line, prefixS) ;});

  return wrap(result);
}

std::string add_pr(const std::string& line, const std::string& prefix) {
  checkUserInterrupt();
  std::ostringstream stream;

  bool last_char_is_space = true;
  for (char current_char: line) {
    if (last_char_is_space && std::isalnum(current_char)) {
      stream << prefix;
      last_char_is_space = false;
    } else if (!last_char_is_space && std::isspace(current_char)) {
      last_char_is_space = true;
    }
    stream << current_char;
  }
  return stream.str();
}
