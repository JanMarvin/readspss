#include <Rcpp.h>
using namespace Rcpp;

//' creates a factor inspired by an idea of Kevin Ushey in the Rcpp gallery
//' http://gallery.rcpp.org/articles/fast-factor-generation/
//'
//' @param x vector
//' @param y charactervector with labels
//' @import Rcpp
//' @export

template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x,
                                    const Vector<RTYPE>& y) {
  Vector<RTYPE> levs = sort_unique(y);
  IntegerVector out = match(x, levs);

  // if (!anyNA)
  //   out[is_na(x)] = NA_INTEGER;
  // else{
  //   // R cannot assing a lable to an NA factor. So we just pick the last value
  //   // of the sort_unique values, which is the NA value. This will be replaced
  //   // with its position in levs, which is identical to length(levs).
  //   int lsize = levs.size();
  //   out[is_na(x)] = lsize;
  // }

  out.attr("levels") = y.attr("names");
  out.attr("class") = "factor";
  return out;
}

// [[Rcpp::export]]
SEXP fast_factor( SEXP x , SEXP y) {
  switch( TYPEOF(x) ) {
  case INTSXP: return fast_factor_template<INTSXP>(x, y);
  case REALSXP: return fast_factor_template<REALSXP>(x, y);
  case STRSXP: return fast_factor_template<STRSXP>(x, y);
  }
  return R_NilValue;
}
