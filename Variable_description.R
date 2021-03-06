#' ---
#' title: "Variable description"
#' author: "Daniel Gil"
#' date: "Sep 2018"
#' output: github_document
#' ---
#' 
#' **SeqDB**<br>
#' **Parameters:** <br>
#' *des:* Optimal design (matrix) <br>
#' *cand.set:* All possible treatments (matrix)<br>
#' *n.alts:* Number of alternatives for each set (numeric)<br>
#' *par.draws:* Draws from the posterior (matrix)<br>
#' *prior.covar:* Prior covariance matrix (Multinormal) (matrix)<br>
#' *weights:* Weights from importance sampling algorithm (numeric)<br>
#' 
#' **Defined Variables** <br>
#' *n.sets:* number of sets<br>
#' *cte.des:* save the constants used in each set<br>
#' *des.f:* transform *des* to a dataframe with dplyr<br>
#' *alt.cte:* subset of variables that contains ".cte" from the optimal design
#' 
#' **Initializing variables** <br>
#' *i.cov:* Inverse of prior covariance matrix <br>
#' *d.start:* Calculate the D-error for each alternative <br>
#' *db.start:* Calculates the mean D-error <br>
#' *full.comb:* Calculates all possible combinations without repetition <br>
#' *n.par:* Number of parameters
#' 
#' **Derr**<br>
#' **Parameters:** <br>
#' *par:* parameter values (posterior draws) <br>
#' *des:* Optimal design (matrix) <br>
#' *n.alts:* Number of alternatives for each set (numeric)<br>
#' 
#' **Defined Variables** <br>
#' *info.des:* Calculates fisher information matrix <br>
#' *detinfo:* Calculates the determinant of the fisher information matrix
#' 
#' **Output** <br>
#' Returns the D-error (numeric)<br>
#' 
#' **InfoDes**<br>
#' **Parameters:** <br>
#' *par:* parameter values (posterior draws). Since it is used in an apply function, the function is evaluated for each row of the posterior (vector) <br>
#' *des:* Optimal design (matrix) <br>
#' *n.alts:* Number of alternatives for each set (numeric)<br>
#' 
#' **Defined variables** <br>
#' *group:* Vector to indicate the choice set (ex: 1 1 2 2 3 3 4 4) <br>
#' *u:* Result of the multiplication of the design matrix with the posterior values (X'Beta). Then a sum of each row(alternative) is done
#' *p:* Estimated probability for each alternative
#' *info.des: *Information matrix
#' 
#' **Output** <br>
#' Returns the Information matrix (matrix)<br>
#' 
#' **DBerrS**<br>
#' **Parameters:** <br>
#' *full.comb:* Calculates all possible combinations without repetition (Matrix)<br>
#' *cand.set:* All possible treatments (matrix)<br>
#' *par.draws:* Draws from the posterior (matrix)<br>
#' *des:* Optimal design (matrix) <br>
#' *n.alts:* Number of alternatives for each set (numeric)<br>
#' *cte.des:* save the constants used in each set (matrix or vector) not sure yet<br>
#' *i.cov:* Inverse of prior covariance matrix (Matrix) <br>
#' *n.par:* Number of parameters (integer) <br>
#' *weights:* Weights from importance sampling algorithm (numeric vector)<br>
#' 
#' **Defined Variables** <br>
#' *set:* # matrix with only the alternatives chosen from the list of all possible treatments/alternatives (Matrix) <br>
#' *d.errors:* Calculate the d.error for each draw of the posterior
#' 
#' 
#'  **DerrS**<br>
#' **Parameters:** <br>
#' *par.draws:* Draws from the posterior (matrix). In c++ is *par*. <br>
#' *set:* # matrix with only the alternatives chosen from the list of all possible treatments/alternatives (Matrix) <br>
#' *des:* Optimal design (matrix) <br>
#' *n.alts:* Number of alternatives for each set (numeric). In c++ is*n_alts*.<br>
#' *i.cov:* Inverse of prior covariance matrix (Matrix). In c´++ is *inv_cov* <br>
#' *n.par:* Number of parameters (integer). In c++ is *n_par*.<br>
#' 
#' **Defined Variables** <br>
#' *des.f:* Append of optimal design with new alternatives <br>
#' *info.d:* Information matrix for each row <br>
#' *d.error* Calculate sequential d-error
#' 
#' **Functions defined**<br>
#' **Originals**<br>
#' *SeqDB:* <br>
#' *Derr:*<br>
#' *DerrS:*<br>
#' *DBerrS:*<br>
#' *InfoDes:*<br>
#' 
#' **Modified**<br>
#' *InfoDes_cpp:* Is the implementation of *InfoDes* in Rcpp<br> 
#' *Derr2:* Is the same function as the original, the only difference is that call *InfoDes_cpp* instead of *InfoDes*<br>
#' *Derr3:* Is the same function as the *Derr2*, but the determinant is computed with *det_cpp* <br>
#' *DerrS2:*Is the same function as the original, the only difference is that call *InfoDes_cpp* instead of *InfoDes*<br>
#' *DerrS3:* Is the same function as the *DerrS2*, but the determinant is computed with *det_cpp* <br>
#' *DBerrS2:*Is the same function as the original, the only difference is that call *InfoDes_cpp* instead of *InfoDes*<br>
#' *DBerrS3:*Is the same function as the *DBerrS3*, but the D-error is computed with *DerrS_cpp*<br>
#' #' *DBerrS4:*Is the same function as the *DBerrS3*, but the D-error is computed with *DerrS3* (using base:rbind)<br>
#' *SeqDB2:* Is the same function as the original, the only difference is that call *InfoDes_cpp* instead of *InfoDes*<br>
#' *SeqDB3:* Is the same function as the original, calls *InfoDes_cpp* and *DBerrS3*, which implies *DerrS_cpp*<br>
#' *SeqDB4:* Is the same function as the original, calls *InfoDes_cpp* and *DBerrS4*, which implies *DerrS3* (uses base:rbind and *det_cpp*)<br>
#' *SeqDB4:* Is the same function as the original, calls *InfoDes_cpp* and *DBerrS4*, which implies *DerrS3* (uses base:rbind and *det_cpp*), and also calls *Derr3*, where it uses det_cpp<br>
