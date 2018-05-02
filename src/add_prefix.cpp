#include <Rcpp.h>
using namespace Rcpp;

static const std::string SPACE = " ";

std::string add_pr(const std::string& line, const std::string& prefix);

//' Add a prefix to each word
//' 
//' Add a custom prefix to each word of a a line.
//' Number of spaces are normalized in the output.
//' Code in C++ (efficient).
//'
//' @param texts a [character] containing the original text
//' @param unit [character] containing the prefix to add (length == 1) or [character] with same length than texts
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
  
  for (int i = 0; i < texts.size(); ++i) {
    if (!unique_prefix) {
      current_prefix = as<std::string>(prefix[i]);
    }
    result[i] = add_pr(as<std::string>(texts[i]), current_prefix);
  }
  return wrap(result);
}

std::string add_pr(const std::string& line, const std::string& prefix) {
  std::stringstream stream;
  std::istringstream iss(line);
  
  std::transform(std::istream_iterator<std::string>(iss),
                 std::istream_iterator<std::string>(),
                 std::ostream_iterator<std::string>(stream, SPACE.c_str()),
                 [&](std::string word) { return word.insert(0, prefix);});
  
  std::string s = stream.str();
  // remove the trailing space
  s.pop_back();
  return s;
}