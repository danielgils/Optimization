Variable description
================
Daniel Gil
Sep 2018

**SeqDB**<br> **Parameters:** <br> *des:* Optimal design (matrix) <br> *cand.set:* All possible treatments (matrix)<br> *n.alts:* Number of alternatives for each set (numeric)<br> *par.draws:* Draws from the posterior (matrix)<br> *prior.covar:* Prior covariance matrix (Multinormal) (matrix)<br> *weights:* Weights from importance sampling algorithm (numeric)<br>

**Defined Variables** <br> *n.sets:* number of sets<br> *cte.des:* save the constants used in each set<br> *des.f:* transform *des* to a dataframe with dplyr<br> *alt.cte:* subset of variables that contains ".cte" from the optimal design

**Initializing variables** <br> *i.cov:* Inverse of prior covariance matrix <br> *d.start:* Calculate the D-error for each alternative <br> *db.start:* Calculates the mean D-error <br> *full.comb:* Calculates all possible combinations without repetition <br> *n.par:* Number of parameters

**Derr**<br> **Parameters:** <br> *par:* parameter values (posterior draws) <br> *des:* Optimal design (matrix) <br> *n.alts:* Number of alternatives for each set (numeric)<br>

**Defined Variables** <br> *info.des:* Calculates fisher information matrix <br> *detinfo:* Calculates the determinant of the fisher information matrix

**Output** <br> Returns the D-error (numeric)<br>

**InfoDes**<br> **Parameters:** <br> *par:* parameter values (posterior draws). Since it is used in an apply function, the function is evaluated for each row of the posterior (vector) <br> *des:* Optimal design (matrix) <br> *n.alts:* Number of alternatives for each set (numeric)<br>

**Defined variables** <br> *group:* Vector to indicate the choice set (ex: 1 1 2 2 3 3 4 4) <br> *u:* Result of the multiplication of the design matrix with the posterior values (X'Beta). Then a sum of each row(alternative) is done *p:* Estimated probability for each alternative *info.des: *Information matrix

**Output** <br> Returns the Information matrix (matrix)<br>

**DBerrS**<br> **Parameters:** <br> *full.comb:* Calculates all possible combinations without repetition (Matrix)<br> *cand.set:* All possible treatments (matrix)<br> *par.draws:* Draws from the posterior (matrix)<br> *des:* Optimal design (matrix) <br> *n.alts:* Number of alternatives for each set (numeric)<br> *cte.des:* save the constants used in each set (matrix or vector) not sure yet<br> *i.cov:* Inverse of prior covariance matrix (Matrix) <br> *n.par:* Number of parameters (integer) <br> *weights:* Weights from importance sampling algorithm (numeric vector)<br>

**Defined Variables** <br> *set:* \# matrix with only the alternatives chosen from the list of all possible treatments/alternatives (Matrix) <br> *d.errors:* Calculate the d.error for each draw of the posterior

' **DerrS**<br>
===============

**Parameters:** <br> *par.draws:* Draws from the posterior (matrix)<br> *set:* \# matrix with only the alternatives chosen from the list of all possible treatments/alternatives (Matrix) <br> *des:* Optimal design (matrix) <br> *n.alts:* Number of alternatives for each set (numeric)<br> *i.cov:* Inverse of prior covariance matrix (Matrix) <br> *n.par:* Number of parameters (integer) <br>

**Defined Variables** <br> *des.f:* Append of optimal design with new alternatives <br> *info.d:* Information matrix for each row <br> *d.error* Calculate sequential d-error

**Functions defined**<br> **Originals**<br> *SeqDB:* <br> *Derr:*<br> *DerrS:*<br> *DBerrS:*<br> *InfoDes:*<br>

**Modified**<br> *InfoDes\_cpp:* Is the implementation of *InfoDes* in Rcpp<br> *Derr2:* Is the same function as the original, the only difference is that call *InfoDes\_cpp* instead of *InfoDes*<br> *DerrS2:*Is the same function as the original, the only difference is that call *InfoDes\_cpp* instead of *InfoDes*<br> *DBerrS2:*Is the same function as the original, the only difference is that call *InfoDes\_cpp* instead of *InfoDes*<br> *SeqDB2:* Is the same function as the original, the only difference is that call *InfoDes\_cpp* instead of *InfoDes*<br>
