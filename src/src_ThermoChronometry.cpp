// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   src_TheromChronometry()
// Author:  Sebastian Kreutzer, Institut of Geography, Heidelberg University (Germany)
// Contact: sebastian.kreutzer@uni-heidelberg.de
// Version: 0.1.0 [2025-06-04]
// Usage:   use with IsothermalHolding.R
// Code and model structure from OpenAI ChatGPT, 2025-06
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

// link to Rcpp
#include <Rcpp.h>
using namespace Rcpp;

// Helper function: Simpson's rule for 1D definite integration
double integrate_simpson(std::function<double(double)> f, double a, double b, int n = 100) {
  if (n % 2 == 1) n++;
  double h = (b - a) / n;
  double s = f(a) + f(b);

  for (int i = 1; i < n; i += 2)
    s += 4 * f(a + i * h);

  for (int i = 2; i < n; i += 2)
    s += 2 * f(a + i * h);

  return s * h / 3.0;
}

// [[Rcpp::export]]
NumericVector f_BTS_cpp_part(
    NumericVector x,
    double A,
    double Eu,
    double s10,
    double Et,
    double kB,
    double T_K,
    double DeltaE,
    double rhop
) {
  int nx = x.size();
  NumericVector out(nx);

  for (int i = 0; i < nx; ++i) {
    double t = x[i];
    // precompute outside the integral for this t
    double outer = std::exp(-rhop * std::pow(std::log(1.8 * 3e15 * (250.0 + t)), 3.0));

    // define integrand as a lambda
    std::function<double(double)> integrand = [&](double Eb) {
      double exp1 = std::exp(-Eb / Eu);
      double exp2 = std::exp(-std::pow(10.0, s10) * t * std::exp(-(Et - Eb) / (kB * T_K)));
      return A * exp1 * exp2;
    };

    double integral = integrate_simpson(integrand, 0.0, DeltaE, 100);

    out[i] = outer * integral;
  }
  return out;
}
