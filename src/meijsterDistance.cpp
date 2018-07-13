#include <Rcpp.h>
#include <functional>
using namespace Rcpp;


int edtFun(int x, int i, int gI){
  return(
    std::pow((x-i), 2) + std::pow(gI, 2)
  );
}

int mdtFun(int x, int i, int gI){
  return(
    std::abs(x-i) + gI
  );
}

int cdtFun(int x, int i, int gI){
  return(
    std::max(std::abs(x-i), gI)
  );
}

double edtSep(double i, double x, int gI, int gX, int maxVal){
  return(
    std::floor(
      (std::pow(x, 2) - std::pow(i, 2) + std::pow(gX, 2) - std::pow(gI, 2)) / (2*(x - i))
    )
  );
}

double mdtSep(double i, double x, int gI, int gX, int maxVal){
  if(gX >= (gI + x - i)){
    return(
      maxVal
    );
  } else if(gI > (gX + x - i)){
    return(
      -maxVal
    );
  } else{
    return(std::floor((gX - gI + x + i) /2));
  }
}

double cdtSep(double i, double x, int gI, int gX, int maxVal){
  if(gI <= gX){
    return(
      std::max(i + gX, floor((i + x)/2))
    );
  } else{
    return(
      std::min(x - gI, floor((i + x)/2))
    );
  }
}

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericMatrix meijsterDistanceC(NumericMatrix mat, String method){

  int cols = mat.ncol();
  int rows = mat.nrow();
  int maxVal = cols+rows;
  NumericMatrix temp(rows, cols);
  NumericMatrix out(rows, cols);

  std::function<int(int, int, int)> fun;
  std::function<double(double, double, int, int, int)> sep;

  if(method=="euclidean"){
    fun = edtFun;
    sep = edtSep;
  } else if(method=="manhattan"){
    fun = mdtFun;
    sep = mdtSep;
  } else if(method=="chessboard"){
    fun = cdtFun;
    sep = cdtSep;
  }

  // first phase
  for(int x = 0; x < cols; x++){

    if(mat(0, x)==1){
      temp(0, x) = 0;
    } else {
      temp(0, x) = maxVal;
    }

    // scan 1
    for(int y = 1; y <= rows-1; y++){
      if(mat(y, x)==1){
        temp(y, x) = 0;
      } else{
        temp(y, x) = 1 + temp(y-1, x);
      }
    }

    // scan 2
    for(int y = rows-2; y >= 0; y--){
      if(temp(y+1, x) < temp(y, x)){
        temp(y, x) = (1 + temp(y+1, x));
      }
    }
  }

  // second phase
  for(int y = 0; y <= rows-1; y++){

    NumericVector s(cols);
    NumericVector t(cols);

    int q = 0;
    s[0] = 0;
    t[0] = 0;

    // scan 3
    for(int x = 1; x <= cols-1; x++){

      int Fi = fun(t[q], s[q], temp(y, s[q]));
      int Fu = fun(t[q], x, temp(y, x));

      while((q >= 1) & (Fi > Fu)){
        q--;
        Fi = fun(t[q], s[q], temp(y, s[q]));
        Fu = fun(t[q], x, temp(y, x));
      }
      if(q < 0){
        q = 0;
        s[q] = x;
      } else{

        int w = 1 + sep(s[q], x, temp(y, s[q]), temp(y, x), maxVal);
        if(w < cols){
          q++;
          s[q] = x;
          t[q] = w;
        }
      }
    }

    // scan 4
    for(int x = cols-1; x >= 0; x--){
      int dis = fun(x, s[q], temp(y, s[q]));
      out(y, x) = dis;
      if(x == t[q]){
        q--;
      }
    }
  }


  return(out);
}