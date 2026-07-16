// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   Collection of fit functions and models in C++
// Author:  F2.1 Geophysical Parametrisation/Regionalisation,
// LIAG - Institute for Applied Geophysics (Hannover, Germany)
// Contact: sebastian.kreutzer@uni-heidelberg.de
// Version: 0.1.1 [2026-07-16]
// The purpose of this function is the reduce the overhead when used in
// with minpack.lm
// This function was adapted with assistance from ChatGPT (OpenAI)
// https://openai.com/chatgpt
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <Rcpp.h>
using namespace Rcpp;

// fit_DoseResponseCurve() =====================================================
// fit.functionSSE equivalent
// [[Rcpp::export]]
NumericVector fit_functionSSE_cpp(double N, double D0, double Di, NumericVector x) {
  int n = x.size();
  NumericVector y(n);

  for(int i = 0; i < n; ++i) {
    y[i] = N * (1.0 - std::exp(-(x[i] + Di) / D0));
  }

  return y;
}

// fit.functionSSELIN equivalent
// [[Rcpp::export]]
NumericVector fit_functionSSELIN_cpp(double N, double D0, double Di, double g, NumericVector x) {
  int n = x.size();
  NumericVector y(n);

  for (int i = 0; i < n; ++i) {
    y[i] = N * (1.0 - std::exp(-(x[i] + Di) / D0) + g * x[i]);
  }

  return y;
}

// fit.functionDSE equivalent
// [[Rcpp::export]]
NumericVector fit_functionDSE_cpp(double N1, double N2, double D01, double D02, NumericVector x) {
  int n = x.size();
  NumericVector y(n);

  for (int i = 0; i < n; ++i) {
    double exp1 = 1.0 - std::exp(-x[i] / D01);
    double exp2 = 1.0 - std::exp(-x[i] / D02);
    y[i] = N1 * exp1 + N2 * exp2;
  }

  return y;
}

// fit.functionGOK equivalent
// [[Rcpp::export]]
NumericVector fit_functionGOK_cpp(double a, double D0, double c, double d, NumericVector x) {
  int n = x.size();
  NumericVector y(n);

  double c_over_D0 = c / D0;
  double exponent = -1.0 / c;
  for (int i = 0; i < n; ++i) {
    double base = 1.0 + (x[i] * c_over_D0);
    y[i] = a * (d - std::pow(base, exponent));
  }

  return y;
}
