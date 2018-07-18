#include <Rcpp.h>
using namespace Rcpp;

// https://stackoverflow.com/questions/8596125/is-there-a-name-for-this-sampling-algorithm-used-in-minicraft
// http://www.paulboxley.com/blog/2011/03/terrain-generation-mark-one

// [[Rcpp::export]]
NumericMatrix diamondSquareC(NumericMatrix mat, NumericVector stepSize, double roughness, double startDev){

  const int n = stepSize.size();

  // the algo is recursive, so we go here through each of its steps
  for(int z = 0; z < n; ++z){
    int halfStep = stepSize[z]/2;
    int aStep = stepSize[z];
    int steps = stepSize[0]/stepSize[z];

    // diamond step
    // In this step the point in the center of a square is computed. This is 
    // always a cell that exists and we can calculate the simple mean of the 
    // corner values plus deviation.
    // a     b
    // 
    //    X
    // 
    // c     d
    for(int x = 0; x < stepSize[0]; x += aStep){
      for(int y = 0; y < stepSize[0]; y += aStep){
        float a = mat(y, x);
        float b = mat(y, x + aStep);
        float c = mat(y + aStep, x);
        float d = mat(y + aStep, x + aStep);
        NumericVector diam = (a + b + c + d)/4 + rnorm(1, 0, startDev);
        mat(y + halfStep, x + halfStep) = diam[0];
      }
    }
    
    // square step
    // In this step we calculate two points in the center of a diamond. The 
    // base cell (a) is the same as in the previous step. e and f can be outsde 
    // of the array. In these cases we need to assign them a value manually. 
    // Here this is the average of the other three values with which e or f 
    // would be averaged otherwise. Hence, the new value of e and f is 
    // determined by their three respective neighbours only.
    //       e
    // 
    //    a  X  b
    //  
    // f  X  d     
    // 
    //    c     
    // 
    //       
    for(int x = 0; x < stepSize[0]; x += aStep){
      for(int y = 0; y < stepSize[0]; y += aStep){
        float a = mat(y, x);
        float b = mat(y, x + aStep);
        float c = mat(y + aStep, x);
        
        float d = mat(y + halfStep, x + halfStep);
        float e = y - halfStep < 0 ? (a + b + d)/3 : mat(y - halfStep, x + halfStep);
        float f = x - halfStep < 0 ? (a + c + d)/3 : mat(y + halfStep, x - halfStep);

        NumericVector square1 = (a + b + d + e)/4 + rnorm(1, 0, startDev);
        NumericVector square2 = (a + c + d + f)/4 + rnorm(1, 0, startDev);
        mat(y, x + halfStep) = square1[0];
        mat(y + halfStep, x) = square2[0];
      }
    }
    startDev = startDev/2;
    
  }

  return mat;
}
