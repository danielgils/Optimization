// [[Rcpp::depends(RcppArmadillo)]]

# include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
double DerrS_cpp(NumericVector par, NumericMatrix set, NumericMatrix des,
                        double n_alts, NumericMatrix inv_cov, double n_par) {
  //https://github.com/petewerner/misc/wiki/RcppArmadillo-cheatsheet#create
  // des.f <- rbind(des, set)  # Append of optimal design with new alternatives
  arma::mat des_arma(des.begin(), des.nrow(), des.ncol(), false);
  arma::mat set_arma(set.begin(), set.nrow(), set.ncol(), false);
  arma::mat des_f = join_cols(des_arma, set_arma);
  
 // Another way to do the rbind: (It's slower)
 /*NumericMatrix out = no_init_matrix(des.nrow()+set.nrow(), des.ncol());
 for (int j = 0; j < des.nrow()+set.nrow(); j++) {
   if (j < des.nrow()) {
     out(j, _) = des(j, _);
   } else {
     out(j, _) = set(j - des.nrow(), _);
   }
 }*/
 //Rcout << "arma:" << des_f << std::endl;
 //Rcout << "std:" << out << std::endl;
 
  // Call function InfoDes_cpp
  //info.d <- InfoDes(par = par.draws, des = des.f, n.alts = n.alts) 
  //NumericMatrix info_d(des.ncol(),des.ncol());
  Function InfoDes_cpp( "InfoDes_cpp" ) ; 
  NumericMatrix info_d = InfoDes_cpp(par,wrap(des_f),n_alts);
  
  // Calculate determinant
  //d.error <- det(info.d + i.cov)^(-1 / n.par)
  arma::mat info_d_arma(info_d.begin(), info_d.nrow(), info_d.ncol(), false);
  arma::mat inv_cov_arma(inv_cov.begin(), inv_cov.nrow(), inv_cov.ncol(), false);
  arma::mat sum_1 = info_d_arma + inv_cov_arma;
  double det = arma::det(sum_1);
  double d = pow(det,(-1 / n_par));
  //Rcout << "det= "<<det<<std::endl;
  //Rcout << "d= "<<d<<std::endl;
  //double det = pow(det(sum_1),(-1 / n_par);
  return(d);
  //return(wrap(sum_1));
  //return(info_d);
}

 
 
 // [[Rcpp::export]]
 double det_cpp(NumericMatrix set) {
   arma::mat set_arma(set.begin(), set.nrow(), set.ncol(), false);
   return(arma::det(set_arma));
 }


#include <Rcpp.h>
// DerrS_cpp
double DerrS_cpp(NumericVector par, NumericMatrix set, NumericMatrix des, double n_alts, NumericMatrix inv_cov, double n_par);
RcppExport SEXP sourceCpp_1_DerrS_cpp(SEXP parSEXP, SEXP setSEXP, SEXP desSEXP, SEXP n_altsSEXP, SEXP inv_covSEXP, SEXP n_parSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type par(parSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type set(setSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type des(desSEXP);
    Rcpp::traits::input_parameter< double >::type n_alts(n_altsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type inv_cov(inv_covSEXP);
    Rcpp::traits::input_parameter< double >::type n_par(n_parSEXP);
    rcpp_result_gen = Rcpp::wrap(DerrS_cpp(par, set, des, n_alts, inv_cov, n_par));
    return rcpp_result_gen;
END_RCPP
}
// det_cpp
double det_cpp(NumericMatrix set);
RcppExport SEXP sourceCpp_1_det_cpp(SEXP setSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type set(setSEXP);
    rcpp_result_gen = Rcpp::wrap(det_cpp(set));
    return rcpp_result_gen;
END_RCPP
}
