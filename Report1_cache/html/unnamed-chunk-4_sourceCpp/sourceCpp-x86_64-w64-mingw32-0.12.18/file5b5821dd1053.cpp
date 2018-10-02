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
