// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   Collection of fit functions and models in C++
// Author:  Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
// Contact: sebastian.kreutzer@uni-heidelberg.de
// Version: 0.1.0 [2025-04-19]
// The purpose of this function is the reduce the overhead when used in
// with minpack.lm
// This function was adapted with assistance from ChatGPT (OpenAI)
// https://openai.com/chatgpt
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <Rcpp.h>
using namespace Rcpp;

// fit_DoseResponseCurve() =====================================================
// fit.functionEXP equivalent
// [[Rcpp::export]]
NumericVector fit_functionEXP_cpp(double a, double b, double c, NumericVector x) {
  int n = x.size();
  NumericVector y(n);

  for(int i = 0; i < n; ++i) {
    y[i] = a * (1.0 - exp(-(x[i] + c) / b));
  }

  return y;
}
// fit.functionEXPLIN equivalent
// [[Rcpp::export]]
NumericVector fit_functionEXPLIN_cpp(double a, double b, double c, double g, NumericVector x) {
  int n = x.size();
  NumericVector y(n);

  for (int i = 0; i < n; ++i) {
    y[i] = a * (1.0 - std::exp(-(x[i] + c) / b) + g * x[i]);
  }

  return y;
}
// fit.functionEXPEXP equivalent
// [[Rcpp::export]]
NumericVector fit_functionEXPEXP_cpp(double a1, double a2, double b1, double b2, NumericVector x) {
  int n = x.size();
  NumericVector y(n);

  for (int i = 0; i < n; ++i) {
    double exp1 = 1.0 - std::exp(-x[i] / b1);
    double exp2 = 1.0 - std::exp(-x[i] / b2);
    y[i] = a1 * exp1 + a2 * exp2;
  }

  return y;
}
// fit.functionGOK equivalent
// [[Rcpp::export]]
NumericVector fit_functionGOK_cpp(double a, double b, double c, double d, NumericVector x) {
  int n = x.size();
  NumericVector y(n);

  for (int i = 0; i < n; ++i) {
    double base = 1.0 + (x[i] * c / b);
    double exponent = -1.0 / c;
    y[i] = a * (d - std::pow(base, exponent));
  }

  return y;
}
