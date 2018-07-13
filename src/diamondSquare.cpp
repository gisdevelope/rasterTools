#include <Rcpp.h>
using namespace Rcpp;

// I chose to use fracdim (fractal dimension) instead of the hurst exponent
// here, because fractal dimension is easier to imagine (at least for me). Since
// the fractal dimension D = 3 - H, H can easily be replaced by D. fracdim is then
// restricted to be a value between 2 and 3. 2 means that the resulting
// heightmap tends towards a plane, while a value of 3 lets the heightmap tend
// towrads maximum randomness.
//
// Travis JMJ, Dytham C. A method for simulating patterns of habitat
// availability at static and dynamic range margins. Oikos. 2004;410–416.
//
// 'range' can be used to set the standard deviation of the random values. This
// has been demonstrated in Palmer MW. The coexistence of species in fractal
// landscapes. The American Naturalist. 1992;139:375–397.
//
// modified after: https://pastebin.com/bgha3zqA

// [[Rcpp::export]]
double makeNoiseC(float d) {
  return (d * (10000 - rand() % (2 * 10000) )) / 10000; // why 10000 here?
}

// [[Rcpp::export]]
NumericMatrix diamondC(NumericMatrix mat, NumericVector where, int distance, float noise){

  const int n = where.size();
  int half = distance / 2;

  for(int x = 0; x < n; ++x) {
    for(int y = 0; y < n; ++y) {
      int px = where[x]-1;
      int py = where[y]-1;
      float p = mat(py, px);
      float b = mat(py, px+distance);
      float r = mat(py+distance, px);
      float br = mat(py+distance, px+distance);

      mat(py+half, px+half) = (p + b + r + br) / 4 + makeNoiseC(noise);

    }
  }

  return mat;
}


// [[Rcpp::export]]
NumericMatrix squareC(NumericMatrix mat, NumericVector where, int distance, float noise){

  const int n = where.size();
  int nrow = mat.nrow(), ncol = mat.ncol();
  int half = distance / 2;

  for(int x = 0; x < n; ++x){
    for(int y = 0; y < n; ++y){
      int px = where[x]-1;
      int py = where[y]-1;
      float p = mat(py+distance, px+distance);
      float b = mat(py, px+distance);
      float r = mat(py+distance, px);
      float br = mat(py, px);
      float hbr = mat(py+half, px+half);

      float ro = px-half < 0 ? 0.0 : mat(py+half, px-half);
      float bo = py-half < 0 ? 0.0 : mat(py-half, px+half);
      float lo = px+half+distance > ncol-1 ? 0.0 : mat(py+half, px+half+distance);
      float to = py+half+distance > nrow-1 ? 0.0 : mat(py+half+distance, px+half);

      mat(py, px+half) = (b + br + hbr + bo) / 4 + makeNoiseC(noise);
      mat(py+distance, px+half) = (p + r + hbr + to) / 4 + makeNoiseC(noise);
      mat(py+half, px) = (r + br + hbr + ro) / 4 + makeNoiseC(noise);
      mat(py+half, px+distance) = (p + b + hbr + lo) / 4 + makeNoiseC(noise);

    }
  }

  return mat;
}