Variable description
================
Daniel Gil
Sep 2018

**SeqDB**<br> **Parameters:** <br> *des:* Optimal design (matrix) <br> *cand.set:* All possible treatments (matrix)<br> *n.alts:* Number of alternatives for each set (numeric)<br> *par.draws:* Draws from the posterior (matrix)<br> *prior.covar:* Prior covariance matrix (Multinormal) (matrix)<br> *weights:* Weights from importance sampling algorithm (numeric)<br>

**Defined Variables** <br> *n.sets:* number of sets<br> *cte.des:* save the constants used in each set<br> *des.f:* transform *des* to a dataframe with dplyr<br> *alt.cte:* subset of variables that contains ".cte" from the optimal design

**Initializing variables** <br> *i.cov:* Inverse of prior covariance matrix<br>

**Derr**<br> **Parameters:** <br> *par:* parameter values (posterior draws) <br> *des:* Optimal design (matrix) <br> *n.alts:* Number of alternatives for each set (numeric)<br>

**Defined Variables** <br>

**InfoDes**<br> **Parameters:** <br> *par:* parameter values (posterior draws). Since it is used in an apply function, the function is evaluated for each row of the posterior (vector) <br> *des:* Optimal design (matrix) <br> *n.alts:* Number of alternatives for each set (numeric)<br>

**Defined variables** <br> *group:* Vector to indicate the choice set (ex: 1 1 2 2 3 3 4 4) <br> *u:* Result of the multiplication of the design matrix with the posterior values (X'Beta). Then a sum of each row(alternative) is done

**Functions defined**<br> **Originals**<br> *SeqDB:* <br> *Derr:*<br> *DerrS:*<br> *DBerrS:*<br> *InfoDes:*<br>

**Modified**<br> *InfoDes\_cpp:* Is the implementation of *InfoDes* in Rcpp<br> *Derr2:* Is the same function as the original, the only difference is that call *InfoDes\_cpp* instead of *InfoDes*<br> *DerrS2:*Is the same function as the original, the only difference is that call *InfoDes\_cpp* instead of *InfoDes*<br> *DBerrS2:*Is the same function as the original, the only difference is that call *InfoDes\_cpp* instead of *InfoDes*<br> *SeqDB2:* Is the same function as the original, the only difference is that call *InfoDes\_cpp* instead of *InfoDes*<br>
