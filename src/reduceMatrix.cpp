#include <Rcpp.h>
using namespace Rcpp;

// NumericMatrix reduceMatrixC(List lMat, Function f) {
//   int n =  lMat.size();
//   NumericMatrix out = lMat[0], mat, mat2;
//   NumericVector theValue;
// 
//   for(int i = 1; i < n; i++){
//     mat = as<NumericMatrix>(lMat[i]);
// 
//     for(int x = 0; x < out.ncol(); x++){
//       for(int y = 0; y < out.nrow(); y++){
//         NumericVector toAdd;
//         toAdd.push_back(out(y, x));
//         toAdd.push_back(mat(y, x));
// 
//         if(is_true(all(is_na(toAdd)))){
//           out(y, x) = NA_REAL;
//         } else{
//           theValue = f(na_omit(toAdd));
//           out(y, x) = theValue[0];
//         }
//       }
//     }
//   }
// 
//   return(out);
// }
