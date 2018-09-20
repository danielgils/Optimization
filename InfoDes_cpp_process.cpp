// [[Rcpp::depends(RcppArmadillo)]]

# include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
//Function with comments and process
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
// u <- des %*% diag(par)  # X'B from the model
// Diagonal matrix for the parameters
//NumericMatrix param(des.ncol(), des.ncol());
//for(i = 0; i < des.ncol(); i++){
//  param(i,i) = par[i];
//}
arma::vec par_arma(par.begin(),des.ncol(),false); //Initializing arma vector 
arma::mat diagonal = arma::diagmat(par_arma);
//return(wrap(diagonal));} // 1:

// Multiplication
//u <- des %*% diag(par)  # X'B from the model
//u <- .rowSums(u, m = nrow(des), n = length(par))  
// https://q-aps.princeton.edu/sites/default/files/q-aps/files/slides_day4_am.pdf
// Create arma objects;
arma::mat des_arma(des.begin(), des.nrow(), des.ncol(), false);
arma::mat u = des_arma * par_arma;
//return(wrap(u.t()));} //2:

arma::mat u_exp = exp(u);
//return(wrap(u_exp.t()));} //3:

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
//Rcout << "n_alts: " <<n_alts<< std::endl;
for(i = 0; i < des.nrow(); i++){
if (cont <= n_alts){
/*Rcout << "if:i: " <<i<< std::endl;
Rcout << "if:cont: " <<cont<< std::endl;
Rcout << "if:index: " <<index<< std::endl;
Rcout << "if:rowsum: " <<rowsum[index]<< std::endl;
Rcout <<  std::endl;*/
//Rcout << "rowsum: " <<rowsum<< std::endl;
   rowsum_rep[i] = rowsum[index];
cont++;
}else{
index++;
/*Rcout << "else:i: " <<i<< std::endl;
Rcout << "else:cont: " <<cont<< std::endl;
Rcout << "else:index: " <<index<< std::endl;
Rcout << "else:rowsum: " <<rowsum[index]<< std::endl;
Rcout <<  std::endl;*/
  rowsum_rep[i] = rowsum[index];
cont=2;
}
}


// Probability
NumericMatrix p(des.nrow(),1);
//Rcout<<"p: "<<p<<" nrow: "<<p.nrow()<<" ncol: "<<p.ncol() << std::endl;
for(i = 0; i < des.nrow(); i++){
p[i] = uexp[i] / rowsum_rep[i];
}
//Rcout<<"uexp: "<<uexp<<" nrow: "<<uexp.nrow()<<" ncol: "<<uexp.ncol() << std::endl;
//Rcout<<"p: "<<p<<" nrow: "<<p.nrow()<<" ncol: "<<p.ncol() << std::endl;


//Rcout<<"des: "<<des<<" nrow: "<<des.nrow()<<" ncol: "<<des.ncol();

// information matrix
//info.des <- crossprod(des * p, des) - crossprod(rowsum( des * p, group))
//  Crossprod 1
// des_p = des*p
NumericMatrix des_p(des.nrow(),des.ncol());
//Rcout<<"des_p: "<< des_p <<" nrow: "<<des_p.nrow()<<" ncol: "<<des_p.ncol();
for(int j = 0; j < des.ncol(); j++){
for( i = 0; i < des.nrow(); i++){
des_p(i,j) = des(i,j) * p[i];
}
}

// crossprod(des * p, des)
arma::mat des_p_arma(des_p.begin(), des_p.nrow(), des_p.ncol(), false);
//Rcout<<"des_arma: "<< des_arma <<" nrow: "<< des_arma.n_rows<< " ncol: " << des_arma.n_cols<<std::endl;
//Rcout<<"des_p_arma: "<< des_p_arma <<" nrow: "<< des_p_arma.n_rows<<" ncol: "<< des_p_arma.n_cols<<std::endl;
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
//Rcout<<"des_arma: "<< des_arma <<" nrow: "<< des_arma.n_rows<< " ncol: " << des_arma.n_cols<<std::endl;
//Rcout<<"des_p_arma: "<< des_p_arma <<" nrow: "<< des_p_arma.n_rows<<" ncol: "<< des_p_arma.n_cols<<std::endl;
arma::mat cross_2 = des_p_rowsum_arma.t() * des_p_rowsum_arma;

// Info.des
//info.des <- crossprod(des * p, des) - crossprod(rowsum( des * p, group))
arma::mat info_des = cross_1 - cross_2;
return(wrap(info_des));
//return(des_p_rowsum);
}


//------------------------------------------------------------



/*Rcout << "arma: "<< u_exp<< std::endl;
Rcout << "std: " <<uexp<< std::endl;
Rcout << "rowsum: " <<rowsum<< std::endl;
*/
//arma::mat rowsum(des.nrow()/n_alts,1);
/*for( i = 0; i < des.nrow(); i++){
//if(index < des.nrow()/n_alts){
index = group[i];
rowsum(index,1) += uexp(i,1);
//Rcout << uexp(i,1) << std::endl;
Rcout << "index=" << index << " and rowsum= " << rowsum(index,1) << " and uexp=" << uexp(i,1) << std::endl;
//}
}*/

/*for( i = 0; i < des.nrow(); i++){
//if(index < des.nrow()/n_alts){
// index = group[i];
//rowsum(index,1) += uexp(i,1);
Rcout << "loop uexp: "<<uexp[0,i] << std::endl;
//Rcout << "index=" << index << " and rowsum= " << rowsum(index,1) << " and uexp=" << uexp(i,1) << std::endl;
//}
                          }
return(uexp);}
*/
/*arma::mat::iterator it_end = u_exp.end();
for(arma::mat::iterator it = u_exp.begin(); it != it_end; ++it)
{
index = group[i];
if( cont <= n_alts){
//rowsum(index) += (*it);
std::cout << "I: "<< it << " rowsum " << rowsum(group[i]) << " and "<< (*it) << std::endl;
cont++;
}else{
index++;
//rowsum(index) += (*it);
std::cout << "I: "<< it << " rowsum " << rowsum(group[i]) << " and "<< (*it) << std::endl;
cont = 2;
}
*/
//rowsum(group[i]) += (*it);
//std::cout << rowsum(group[i]) << " and "<< (*it) << std::endl;
//std::cout << (*it) << std::endl;
//}

/*for( i = 0; i < des.nrow(); i++){
//if(index < des.nrow()/n_alts){
index = group[i];
rowsum(index,1) += uexp(i,1);
//Rcout << uexp(i,1) << std::endl;
Rcout << "index=" << index << " and rowsum= " << rowsum(index,1) << " and uexp=" << uexp(i,1) << std::endl;
//}
}*/
/*for( i = 0; i < des.nrow(); i++){
if( cont <= n_alts){
rowsum(index,1) += u_exp(i,1);
cont++;
}else{
index++;
rowsum(index,1) += u_exp(i,1);
cont = 2;
}
}*/
//return(rowsum);
//return(wrap(rowsum));
//}




//-----------------------------------------------------------------------  
//arma::vec u(nrow,ncol);

/*NumericMatrix u(nrow,ncol);
for (i = 0; i < nrow; i++){
for (j = 0; j < ncol; j++){
for (k = 0; k < ncol; k++){
u(i,j) += des(i,k) * param(k,j);
}
}
}

// sum of each row
// u <- .rowSums(u, m = nrow(des), n = length(par))  
NumericMatrix u_sum(nrow,1);
for (i = 0; i < nrow; i++){
for (j = 0; j < ncol; j++){
u_sum(i,1) += u(i,j);
}
}*/
//return u;
//}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*/*** R
timesTwo(42)
*/
