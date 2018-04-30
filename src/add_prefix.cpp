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
                 [&](String line) { return add_pr(line, prefixS) ;});

  return wrap(result);
}

std::string add_pr(const std::string& line, const std::string& prefix) {
  checkUserInterrupt();
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
