#include <Rcpp.h>
using namespace Rcpp;

// This function has been inspired and influenced greatly by R::mmand::morph
// written by Jon Clayden.

NumericVector blendIdentity(NumericVector &temp, NumericVector &kernel){

  if(any(is_na(kernel))){
    for(int i = 0; i < temp.size(); i++){
      if(is_na(kernel)[i]){
        temp[i] = NA_REAL;
      }
    }
  }

  return(temp);
}

NumericVector blendEqual(NumericVector &temp, NumericVector &kernel){
  NumericVector out(1);
  out[0] = 1.0;

  for(int i = 0; i < temp.size(); i++){
    if(!is_na(kernel)[i] & (temp[i] != kernel[i])){
      out[0] = 0.0;
      break;
    }
  }

  return(out);
}

NumericVector blendLower(NumericVector &temp, NumericVector &kernel){

  for(int i = 0; i < temp.size(); i++){
    if(is_na(kernel)[i]){
      temp.erase(i);
    } else if(temp[i] > kernel[i]){
      temp[i] = 0.0;
    }
  }

  return(temp);
}

NumericVector blendGreater(NumericVector &temp, NumericVector &kernel){

  for(int i = 0; i < temp.size(); i++){
    if(is_na(kernel)[i]){
      temp.erase(i);
    } else if(temp[i] < kernel[i]){
      temp[i] = 0.0;
    }
  }

  return(temp);
}

NumericVector blendPlus(NumericVector &temp, NumericVector &kernel){

  for(int i = 0; i < temp.size(); i++){
    if(is_na(kernel)[i]){
      temp.erase(i);
      kernel.erase(i);
    }
  }

  return(temp + kernel);

}

NumericVector blendMinus(NumericVector &temp, NumericVector &kernel){

  for(int i = 0; i < temp.size(); i++){
    if(is_na(kernel)[i]){
      temp.erase(i);
      kernel.erase(i);
    }
  }

  return(temp - kernel);

}

NumericVector blendProdukt(NumericVector &temp, NumericVector &kernel){

  for(int i = 0; i < temp.size(); i++){
    if(is_na(kernel)[i]){
      temp.erase(i);
      kernel.erase(i);
    }
  }

  return(temp * kernel);

}

double mergeMin(const NumericVector &values){
  return(min(na_omit(values)));
}

double mergeMax(const NumericVector &values){
  return(max(na_omit(values)));
}

double mergeAll(const NumericVector &values){
  if(all(na_omit(values) != 0).is_true()){
    return(1);
  } else{
    return(0);
  }
}

double mergeAny(const NumericVector &values){
  if(any(na_omit(values) != 0).is_true()){
    return(1);
  } else{
    return(0);
  }
}

double mergeNotAll(const NumericVector &values){
  if(all(na_omit(values) != 0).is_true()){
    return(0);
  } else{
    return(1);
  }
}

double mergeNotAny(const NumericVector &values){
  if(any(na_omit(values) != 0).is_true()){
    return(0);
  } else{
    return(1);
  }
}

double mergeSum(const NumericVector &values){
  return(sum(na_omit(values)));
}

double mergeNA(const NumericVector &values){
  if(sum(na_omit(values)) != 0){
    return(sum(values));
  } else{
    return(NA_REAL);
  }
}

double mergeMean(const NumericVector &values){
  return(mean(na_omit(values)));
}

double mergeMedian(const NumericVector &values){
  return(median(na_omit(values)));
}

double mergeSD(const NumericVector &values){
  return(sd(na_omit(values)));
}

double mergeCV(const NumericVector &values){
  return(sd(na_omit(values))/mean(na_omit(values)));
}

// [[Rcpp::plugins(cpp11)]]

using BlendFunctionPtr = NumericVector (*)(NumericVector &, NumericVector &);
using MergeFunctionPtr = double (*)(const NumericVector &);

static BlendFunctionPtr blendFuns[7] = {&blendIdentity, &blendEqual, &blendLower, &blendGreater, &blendPlus, &blendMinus, &blendProdukt};
static MergeFunctionPtr mergeFuns[12] = {&mergeMin, &mergeMax, &mergeAll, &mergeAny, &mergeNotAll, &mergeNotAny, &mergeSum, &mergeMean, &mergeMedian, &mergeSD, &mergeCV, &mergeNA};

// [[Rcpp::export]]
NumericMatrix morphC(NumericMatrix &mat, NumericMatrix &kernel, NumericVector &value, int blend, int merge,
                     bool rotateKernel, bool strictKernel){

  // determine kernel and mat dimensions and margin around the iterator
  int kRows = kernel.nrow(), kCols = kernel.ncol();
  int mRows = mat.nrow(), mCols = mat.ncol();
  int yMar = floor(kRows/2), xMar = floor(kCols/2);

  // determine temporary vector to capture values and output matrix
  NumericVector tempMat, tempKernel;
  NumericMatrix rotatedKernel, kernelToRotate;
  double merged;
  NumericMatrix out = clone(mat);

  // grab functions
  BlendFunctionPtr blendFun = blendFuns[blend-1];
  MergeFunctionPtr mergeFun = mergeFuns[merge-1];

  for(int x = 0; x < mCols; x++){
    for(int y = 0; y < mRows; y++){

      // if target value is not part of the allowed values, skip
      if(!any(mat(y, x) == value).is_true()){
        continue;
      }

      int mR = x+xMar;
      int mL = x-xMar;
      int kR = kRows-1;
      int kL = 0;
      int mT = y-yMar;
      int mB = y+yMar;
      int kT = 0;
      int kB = kCols-1;

      // define kernel restrictions when the iterator forbids full coverage of mat by kernel.
      if(!strictKernel){
        if(mL < 0){
          mL = 0;
          kL = xMar-x;
        } else if(mR > mCols-1){
          mR = mCols-1;
          kR = kCols-1-xMar;
        }
        if(mT < 0){
          mT = 0;
          kT = yMar-y;
        } else if(mB > mRows-1){
          mB = mRows-1;
          kB = kRows-1-yMar;
        }
      }

      // set mat and kernel with restrictions
      tempMat = mat(Range(mT, mB), Range(mL, mR));
      tempKernel = kernel(Range(kT, kB), Range(kL, kR));

      merged = mergeFun(blendFun(tempMat, tempKernel));

      // if merged is already 1 (after blendEqual and mergeAll/Any, which is the
      // only reasonable use-case for rotateKernel = TRUE), we don't need to
      // rotate because we know already that it is a hit.
      if(rotateKernel && merged != 1.0){
        rotatedKernel = clone(kernel);
        // rotate four times
        for(int r = 0; r < 4; r++){
          kernelToRotate = clone(rotatedKernel);
          for(int m = 0; m < kCols; m++){
            // change dimension and revert rows
            rotatedKernel(m, _) = rev(kernelToRotate(_, m));
          }
          // apply restrictions to rotatedKernel
          tempKernel = rotatedKernel(Range(kT, kB), Range(kL, kR));
          merged = mergeFun(blendFun(tempMat, tempKernel));
          if(merged == 1.0){
            break;
          }
        }

      }
      out(y, x) = merged;
    }
  }

  return(out);
}
