#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool isBinaryC(NumericMatrix mat) {
  int data(mat.size());
  bool isBinary = true;
  double nonZero = 0.0;

  for(int i = 0; i < data; i++){
    if(mat[i] != 0.0){
      if((nonZero != 0.0) & (mat[i] != nonZero)){
        isBinary = false;
        break;
      } else{
        nonZero = mat[i];
      }
    }
  }
  return(isBinary);
}
