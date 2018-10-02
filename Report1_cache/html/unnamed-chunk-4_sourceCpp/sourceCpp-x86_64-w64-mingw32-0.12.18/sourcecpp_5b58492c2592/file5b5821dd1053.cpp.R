`.sourceCpp_1_DLLInfo` <- dyn.load('C:/Users/danie/Documents/Daniel Gil/KULeuven/Stage 2/Thesis/Scripts/Optimization/Report1_cache/html/unnamed-chunk-4_sourceCpp/sourceCpp-x86_64-w64-mingw32-0.12.18/sourcecpp_5b58492c2592/sourceCpp_2.dll')

DerrS_cpp <- Rcpp:::sourceCppFunction(function(par, set, des, n_alts, inv_cov, n_par) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_DerrS_cpp')
det_cpp <- Rcpp:::sourceCppFunction(function(set) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_det_cpp')

rm(`.sourceCpp_1_DLLInfo`)
