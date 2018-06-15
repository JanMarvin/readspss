// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// fast_factor
SEXP fast_factor(SEXP x, SEXP y);
RcppExport SEXP _readspss_fast_factor(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(fast_factor(x, y));
    return rcpp_result_gen;
END_RCPP
}
// por
List por(const char * filePath, const bool debug, std::string encStr);
RcppExport SEXP _readspss_por(SEXP filePathSEXP, SEXP debugSEXP, SEXP encStrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const char * >::type filePath(filePathSEXP);
    Rcpp::traits::input_parameter< const bool >::type debug(debugSEXP);
    Rcpp::traits::input_parameter< std::string >::type encStr(encStrSEXP);
    rcpp_result_gen = Rcpp::wrap(por(filePath, debug, encStr));
    return rcpp_result_gen;
END_RCPP
}
// sav
List sav(const char * filePath, const bool debug, std::string encStr, std::string const ownEnc);
RcppExport SEXP _readspss_sav(SEXP filePathSEXP, SEXP debugSEXP, SEXP encStrSEXP, SEXP ownEncSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const char * >::type filePath(filePathSEXP);
    Rcpp::traits::input_parameter< const bool >::type debug(debugSEXP);
    Rcpp::traits::input_parameter< std::string >::type encStr(encStrSEXP);
    Rcpp::traits::input_parameter< std::string const >::type ownEnc(ownEncSEXP);
    rcpp_result_gen = Rcpp::wrap(sav(filePath, debug, encStr, ownEnc));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_readspss_fast_factor", (DL_FUNC) &_readspss_fast_factor, 2},
    {"_readspss_por", (DL_FUNC) &_readspss_por, 3},
    {"_readspss_sav", (DL_FUNC) &_readspss_sav, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_readspss(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
