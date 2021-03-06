// [[Rcpp::depends(RcppArmadillo)]]

# include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix InfoDes_cpp(NumericVector par, NumericMatrix des,
                             double n_alts) {
  int i = 0;
  NumericVector group(des.nrow());
  int ind = 0;
  int cont = 1;
  //group <- rep(seq(1, nrow(des) / n.alts, 1), each = n.alts) #Vector to 
  //indicate the choice set
  // ToDo: try to improve => rep function in cpp: 
  //https://stackoverflow.com/questions/28442582/reproducing-r-rep-with-the-times-argument-in-c-and-rcpp
  for (i = 0; i < des.nrow(); i++){
    if (cont <= n_alts){
      group[i] = ind;
      cont++;
    } else{
      ind++;
      group[i] = ind;
      cont = 2;
    }
  }
  
  // probability
  arma::vec par_arma(par.begin(),des.ncol(),false); //Initializing arma vector 
  arma::mat diagonal = arma::diagmat(par_arma);
  
  // Multiplication
  // Create arma objects;
  arma::mat des_arma(des.begin(), des.nrow(), des.ncol(), false);
  arma::mat u = des_arma * par_arma;
  
  // Exponential
  arma::mat u_exp = exp(u);
  
  // Sum for each choice set
  NumericMatrix uexp = wrap(u_exp);
  NumericMatrix rowsum(des.nrow()/n_alts,1);
  cont = 1;
  int index = 0;
  for( i = 0; i < des.nrow(); i++){
    if( cont <= n_alts){
      rowsum(index,0) += u_exp(i,0);
      cont++;
    }else{
    index++;
    rowsum(index,0) += u_exp(i,0);
    cont = 2;
    }
  }
  
  // Repite each value n_alts times;
  NumericMatrix rowsum_rep(des.nrow(),1);
  cont = 1;
  index = 0 ;
  for(i = 0; i < des.nrow(); i++){
    if (cont <= n_alts){
      rowsum_rep[i] = rowsum[index];
      cont++;
    }else{
      index++;
      rowsum_rep[i] = rowsum[index];
      cont=2;
    }
  }
  
  // Probability
  NumericMatrix p(des.nrow(),1);
  for(i = 0; i < des.nrow(); i++){
    p[i] = uexp[i] / rowsum_rep[i];
  }
  
  // information matrix
  //  Crossprod 1
  // des_p = des*p
  NumericMatrix des_p(des.nrow(),des.ncol());
  for(int j = 0; j < des.ncol(); j++){
    for( i = 0; i < des.nrow(); i++){
      des_p(i,j) = des(i,j) * p[i];
    }
  }
  
  // crossprod(des * p, des)
  arma::mat des_p_arma(des_p.begin(), des_p.nrow(), des_p.ncol(), false);
  arma::mat cross_1 = des_p_arma.t() * des_arma;
  
  //  Crossprod 2
  //rowsum( des * p, group)
  NumericMatrix des_p_rowsum(des_p.nrow()/n_alts,des_p.ncol());
  for(int j=0; j < des_p.ncol(); j++){ 
    cont = 1;
    index = 0;
    for( i = 0; i < des_p.nrow(); i++){
      if( cont <= n_alts){
        des_p_rowsum(index,j) += des_p(i,j);
        cont++;
      }else{
        index++;
        des_p_rowsum(index,j) += des_p(i,j);
        cont = 2;
      }
    }
  }
  
  // crossprod(des * p, des)
  arma::mat des_p_rowsum_arma(des_p_rowsum.begin(), des_p_rowsum.nrow(), des_p_rowsum.ncol(), false);
  arma::mat cross_2 = des_p_rowsum_arma.t() * des_p_rowsum_arma;
  
  // Info.des
  //info.des <- crossprod(des * p, des) - crossprod(rowsum( des * p, group))
  arma::mat info_des = cross_1 - cross_2;
  return(wrap(info_des));
}


#include <Rcpp.h>
// InfoDes_cpp
NumericMatrix InfoDes_cpp(NumericVector par, NumericMatrix des, double n_alts);
RcppExport SEXP sourceCpp_1_InfoDes_cpp(SEXP parSEXP, SEXP desSEXP, SEXP n_altsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type par(parSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type des(desSEXP);
    Rcpp::traits::input_parameter< double >::type n_alts(n_altsSEXP);
    rcpp_result_gen = Rcpp::wrap(InfoDes_cpp(par, des, n_alts));
    return rcpp_result_gen;
END_RCPP
}
