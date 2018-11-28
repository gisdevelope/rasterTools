#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame countCellsC(NumericMatrix &mat) {
  int mRows = mat.nrow(), mCols = mat.ncol(), elements;

  IntegerVector values, theValue, position;
  values = sort_unique(mat);
  elements = values.size();

  IntegerVector cells(elements);

  for(int y = 0; y < mRows; y++){
    for(int x = 0; x < mCols; x++){

      theValue = mat(y, x);
      if(!any(mat(y, x) == values).is_true()){
        continue;
      }

      int position = match(theValue, values)[0];

      cells[position-1] += 1;

    }
  }

  DataFrame out = DataFrame::create(Named("value")=values,
                                    Named("cells")=cells);

  return(out);
}

// [[Rcpp::export]]
DataFrame countEdgesC(NumericMatrix &mat) {
  int mRows = mat.nrow(), mCols = mat.ncol(), elements;

  IntegerVector values, theValue, position;
  values = sort_unique(mat);
  elements = values.size();

  IntegerVector edgesX(elements), edgesY(elements);

  for(int y = 0; y < mRows; y++){
    for(int x = 0; x < mCols; x++){

      theValue = mat(y, x);
      if(!any(mat(y, x) == values).is_true()){
        continue;
      }

      int position = match(theValue, values)[0];

      // making use of the fact that NaN values are not equal to themselves, hence filter out boundaries with NA.
      if(mat(y, x+1) == mat(y, x+1)){
        if(any(theValue != mat(y, x+1)).is_true()){
          if(x != mCols-1){
            edgesX[position-1] += 1;
          }
        }
      }

      if(mat(y+1, x) == mat(y+1, x)){
        if(any(theValue != mat(y+1, x)).is_true()){
          if(y != mRows-1){
            edgesY[position-1] += 1;
          }
        }
      }

      if(mat(y, x-1) == mat(y, x-1)){
        if(any(theValue != mat(y, x-1)).is_true()){
          if(x != 0){
            edgesX[position-1] += 1;
          }
        }
      }

      if(mat(y-1, x) == mat(y-1, x)){
        if(any(theValue != mat(y-1, x)).is_true()){
          if(y != 0){
            edgesY[position-1] += 1;
          }
        }
      }

    }
  }

  DataFrame out = DataFrame::create(Named("value")=values,
                                    Named("edgesX")=edgesX,
                                    Named("edgesY")=edgesY);

  return(out);
}

// [[Rcpp::export]]
NumericMatrix countAdjacenciesC(NumericMatrix &mat, bool countDouble) {
  int mRows = mat.nrow(), mCols = mat.ncol(), elements;

  IntegerVector values, theValue, rightValue, leftValue, topValue, bottomValue, position;
  int posFocal, posRight, posLeft, posBottom;
  values = sort_unique(na_omit(mat));
  elements = values.size();
  // Rcout << values << std::endl;
  NumericMatrix out(elements, elements);

  for(int y = 0; y < mRows; y++){
    for(int x = 0; x < mCols; x++){

      theValue = mat(y, x);
      if(mat(y, x) != mat(y, x)){
        continue;
      }
      posFocal = match(theValue, values)[0];
      
      if(x != mCols-1){
        rightValue = mat(y, x+1);
        if(!any(is_na(rightValue)).is_true()){
          posRight = match(rightValue, values)[0];
          out(posFocal-1, posRight-1) += 1;
        }
      }
      if(y != mRows-1){
        bottomValue = mat(y+1, x);
        if(!any(is_na(bottomValue)).is_true()){
          posBottom = match(bottomValue, values)[0];
          out(posFocal-1, posBottom-1) += 1;
        }
      }

      if(countDouble){
        if(x != 0){
          leftValue = mat(y, x-1);
          if(!any(is_na(leftValue)).is_true()){
            posLeft = match(leftValue, values)[0];
            out(posFocal-1, posLeft-1) += 1;
          }
        }
        if(y != 0){
          topValue = mat(y-1, x);
          if(!any(is_na(topValue)).is_true()){
            posBottom = match(topValue, values)[0];
            out(posFocal-1, posBottom-1) += 1;
          }
        }
      }

    }
  }

  return(out);
}
