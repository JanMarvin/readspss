#include <Rcpp.h>


template <int RTYPE>
Rcpp::IntegerVector fast_factor_template( const Rcpp::Vector<RTYPE>& x,
                                          const Rcpp::Vector<RTYPE>& y) {
  Rcpp::IntegerVector out = match(x, y);

  out.attr("levels") = y.attr("names");
  out.attr("class") = "factor";
  return out;
}

//' creates a factor inspired by an idea of Kevin Ushey in the Rcpp gallery
//' http://gallery.rcpp.org/articles/fast-factor-generation/
//'
//' @param x vector
//' @param y charactervector with labels
//' @import Rcpp
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
SEXP fast_factor( SEXP x , SEXP y) {
  switch( TYPEOF(x) ) {
  case INTSXP: return fast_factor_template<INTSXP>(x, y);
  case REALSXP: return fast_factor_template<REALSXP>(x, y);
  case STRSXP: return fast_factor_template<STRSXP>(x, y);
  }
  return R_NilValue;
}
