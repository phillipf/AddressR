// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// parallelAddressTotalScore
Rcpp::NumericMatrix parallelAddressTotalScore(Rcpp::DataFrame input, Rcpp::DataFrame target);
RcppExport SEXP AddressR_parallelAddressTotalScore(SEXP inputSEXP, SEXP targetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type input(inputSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type target(targetSEXP);
    rcpp_result_gen = Rcpp::wrap(parallelAddressTotalScore(input, target));
    return rcpp_result_gen;
END_RCPP
}
// parallelAddressTotalScore3
Rcpp::NumericMatrix parallelAddressTotalScore3(Rcpp::DataFrame input, Rcpp::DataFrame target);
RcppExport SEXP AddressR_parallelAddressTotalScore3(SEXP inputSEXP, SEXP targetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type input(inputSEXP);
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type target(targetSEXP);
    rcpp_result_gen = Rcpp::wrap(parallelAddressTotalScore3(input, target));
    return rcpp_result_gen;
END_RCPP
}
