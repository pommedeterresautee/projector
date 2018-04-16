// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// average_vectors
NumericMatrix average_vectors(const List& keys, const NumericMatrix& mat, bool na_if_unknwown_word, int threads, bool verbose);
RcppExport SEXP _projector_average_vectors(SEXP keysSEXP, SEXP matSEXP, SEXP na_if_unknwown_wordSEXP, SEXP threadsSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< bool >::type na_if_unknwown_word(na_if_unknwown_wordSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(average_vectors(keys, mat, na_if_unknwown_word, threads, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_projector_average_vectors", (DL_FUNC) &_projector_average_vectors, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_projector(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
