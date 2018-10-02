// [[Rcpp::depends(RcppArmadillo)]]

# include <RcppArmadillo.h>

using namespace Rcpp;

 // [[Rcpp::export]]
 double det_cpp(NumericMatrix set) {
   arma::mat set_arma(set.begin(), set.nrow(), set.ncol(), false);
   return(arma::det(set_arma));
 }



#include <Rcpp.h>
// det_cpp
double det_cpp(NumericMatrix set);
RcppExport SEXP sourceCpp_3_det_cpp(SEXP setSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type set(setSEXP);
    rcpp_result_gen = Rcpp::wrap(det_cpp(set));
    return rcpp_result_gen;
END_RCPP
}
