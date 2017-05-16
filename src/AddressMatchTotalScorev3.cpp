
#include <Rcpp.h>
#include <RcppParallel.h>
#include "edlib.h"

using namespace RcppParallel;


//' @title
//' Address match v3
//' @description
//' Finds the best matches for an address using Levenshtein
//'
//' @param input the input dataframe for cleaning
//'
//' @param target the target dataframe for cleanning
//'
//' @details
//' \code{session_count} takes a vector of intertime values (generated via \code{\link{intertimes}},
//' or in any other way you see fit) and returns the total number of sessions within that dataset.
//' It's implimented in C++, providing a (small) increase in speed over the R equivalent.
//' @export

// [[Rcpp::plugins(cpp11)]]
inline double match(char* x, char* y) {

  int lenx = std::string(x).length();
  int leny = std::string(y).length();

  EdlibAlignResult result = edlibAlign(x, lenx, y, leny, edlibDefaultAlignConfig());

  return double(result.editDistance);

};

inline double diff(int x, int y) {

  return int(std::abs(x - y));

};

struct MatchAddress : public Worker
{
  Rcpp::DataFrame input;
  Rcpp::DataFrame target;
  // source matrix
  // Rcpp::CharacterVector key = input["KEY"];

  Rcpp::CharacterVector input1 = input["STREET_NAME"];
  const Rcpp::CharacterVector target1 = target["STREET_NAME"];

  Rcpp::CharacterVector input2 = input["LOCALITY_NAME"];
  const Rcpp::CharacterVector target2 = target["LOCALITY_NAME"];

  Rcpp::NumericVector input3 = input["NUMBER_FIRST"];
  const Rcpp::NumericVector target3 = target["NUMBER_FIRST"];

  Rcpp::NumericVector input4 = input["NUMBER_LAST"];
  const Rcpp::NumericVector target4 = target["NUMBER_LAST"];

  Rcpp::NumericVector input5 = input["ADDRESS_TYPE"];
  const Rcpp::NumericVector target5 = target["ADDRESS_TYPE"];

  // destination matrix
  RMatrix<double> output;

  // initialize with source and destination
  MatchAddress(Rcpp::DataFrame input, const Rcpp::DataFrame target, Rcpp::NumericMatrix output)
    : input(input), target(target), output(output) {}

  // take the square root of the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      for (std::size_t j = 0; j < target.nrows(); j++) {

        double NumFirstdiff = diff(input3[i], target3[j]);

        if(target1[j] == "NA_STRING") {

          output(i,j) = 99999 + match(input2[i], target2[j]) + NumFirstdiff;

        }

        // RMatrix<double>::Row row1 = mat.row(i);
        if(input5[i] == 1) {

          output(i,j) = match(input1[i], target1[j]) + match(input2[i], target2[j]) + NumFirstdiff;

        }
        if(input5[i] == 2) {

          double NumLastdiff = diff(input4[i], target4[j]);

          if(input3[i] > target3[j] & input3[i] < target4[j]) {

          output(i,j) = match(input1[i], target1[j]) + match(input2[i], target2[j]) + 1;

          } else {

          output(i,j) = match(input1[i], target1[j]) + match(input2[i], target2[j]) + NumFirstdiff + NumLastdiff;

          }

        }

      }

    }

  }
};

// [[Rcpp::export]]
Rcpp::NumericMatrix parallelAddressTotalScore3(Rcpp::DataFrame input, Rcpp::DataFrame target) {

  // allocate the output matrix
  Rcpp::NumericMatrix output(input.nrows(), target.nrows());

  // SquareRoot functor (pass input and output matrixes)
  MatchAddress Matchaddress(input, target, output);

  // call parallelFor to do the work
  parallelFor(0, input.nrows(), Matchaddress);

  // return the output matrix
  return output;
}




/*** R

#library(dplyr)
# test <- Address %>%
#   sample_n(30) %>%
#   distinct() %>%
#   filter(!is.na(LOCA_STREET))

#result <- parallelAddressTotalScore(TW_address[1,], GNAF_address[options2[[1]],])

*/
