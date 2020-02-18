#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector lagAvgTopN(NumericVector values,NumericVector endDates,int n,int window,NumericVector adj){
  int valuesLen = values.length();
  NumericVector out = clone(values);
  NumericVector startDates = endDates - window;

  for (int i = 0; i < valuesLen; ++i){
    LogicalVector idx = (endDates <= endDates[i]) & (endDates >= startDates[i]);
    NumericVector valuesWindow = values[idx];
    int lenCur = valuesWindow.length();

    if (lenCur <= n){
      out[i] = mean(valuesWindow) * adj[lenCur - 1];
    }else{
      std::nth_element(valuesWindow.begin(),valuesWindow.begin() + n,valuesWindow.end());
      out[i] = mean(head(valuesWindow,n));
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector lagMedianAll(NumericVector values,NumericVector endDates,int window,NumericVector adj){
  int valuesLen = values.length();
  int adj_len = adj.length();
  NumericVector out = clone(values);
  NumericVector startDates = endDates - window;

  for (int i = 0; i < valuesLen; ++i){
    LogicalVector idx = (endDates <= endDates[i]) & (endDates >= startDates[i]);
    NumericVector valuesWindow = values[idx];
    int lenCur = valuesWindow.length();

    if (lenCur <= adj_len){
      out[i] = median(valuesWindow) * adj[lenCur - 1];
    }else{
      out[i] = median(valuesWindow);
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector lagMAD(NumericVector values,NumericVector endDates,int window){
  int valuesLen = values.length();
  NumericVector out = clone(values);
  NumericVector startDates = endDates - window;

  for (int i = 0; i < valuesLen; ++i){
    LogicalVector idx = (endDates <= endDates[i]) & (endDates >= startDates[i]);
    NumericVector valuesWindow = values[idx];
    int lenCur = valuesWindow.length();

    if (lenCur == 1){
      out[i] = NA_REAL;
    }else{
      double c = median(valuesWindow);
      valuesWindow = valuesWindow - c;
      valuesWindow = abs(valuesWindow);
      out[i] = median(valuesWindow);
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector lagSD(NumericVector values,NumericVector endDates,int window){
  int valuesLen = values.length();
  NumericVector out = clone(values);
  NumericVector startDates = endDates - window;

  for (int i = 0; i < valuesLen; ++i){
    LogicalVector idx = (endDates <= endDates[i]) & (endDates >= startDates[i]);
    NumericVector valuesWindow = values[idx];
    int lenCur = valuesWindow.length();

    if (lenCur == 1){
      out[i] = NA_REAL;
    }else{
      out[i] = sd(valuesWindow);
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector ptsThresh(NumericVector values,NumericVector endDates,int window){
  int valuesLen = values.length();
  NumericVector out = clone(values);
  NumericVector startDates = endDates - window;

  for (int i = 0; i < valuesLen; ++i){
    LogicalVector idx = (endDates <= endDates[i]) & (endDates >= startDates[i]);
    NumericVector valuesWindow = values[idx];
    int lenCur = valuesWindow.length();

    if (lenCur == 1){
      out[i] = (1.4 / 1.3) * valuesWindow[i];
    }
    if (lenCur == 2){
      out[i] = (1.5 / 2.4) * sum(valuesWindow);
    }
    if (lenCur == 3){
      out[i] = (1.5 / 3.3) * sum(valuesWindow);
    }
    if (lenCur == 4){
      out[i] = (1.5 / 4.0) * sum(valuesWindow);
    }
    if (lenCur >= 5){
      std::nth_element(valuesWindow.begin(),valuesWindow.begin() + 5,valuesWindow.end());
      out[i] = valuesWindow[4];
    }
  }

  return out;
}
