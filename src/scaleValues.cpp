#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix scaleMatrixC(NumericMatrix mat, NumericVector range){
  int mRows = mat.nrow(), mCols = mat.ncol();
  NumericMatrix out = clone(mat);
  double minVal = min(mat);
  double maxVal = max(mat);

  if(minVal == maxVal){
    return(out);
  }
  
  for(int y = 0; y < mRows; y++){
    for(int x = 0; x < mCols; x++){
      out(y, x) = (mat(y, x) - minVal) * (range[1] - range[0]) / (maxVal - minVal) + range[0];
    }
  }
  
  return(out);
}