// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// create_UID
CharacterVector create_UID();
RcppExport SEXP _Luminescence_create_UID() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(create_UID());
    return rcpp_result_gen;
END_RCPP
}
// src_EED_Calc_Overall_StatUncertainty
NumericMatrix src_EED_Calc_Overall_StatUncertainty(NumericMatrix M_Simul, int Ndata, int Nsimul, int MinNbSimExp);
RcppExport SEXP _Luminescence_src_EED_Calc_Overall_StatUncertainty(SEXP M_SimulSEXP, SEXP NdataSEXP, SEXP NsimulSEXP, SEXP MinNbSimExpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type M_Simul(M_SimulSEXP);
    Rcpp::traits::input_parameter< int >::type Ndata(NdataSEXP);
    Rcpp::traits::input_parameter< int >::type Nsimul(NsimulSEXP);
    Rcpp::traits::input_parameter< int >::type MinNbSimExp(MinNbSimExpSEXP);
    rcpp_result_gen = Rcpp::wrap(src_EED_Calc_Overall_StatUncertainty(M_Simul, Ndata, Nsimul, MinNbSimExp));
    return rcpp_result_gen;
END_RCPP
}
// f_BTS_cpp_part
NumericVector f_BTS_cpp_part(NumericVector x, double A, double Eu, double s10, double Et, double kB, double T_K, double DeltaE, double rhop);
RcppExport SEXP _Luminescence_f_BTS_cpp_part(SEXP xSEXP, SEXP ASEXP, SEXP EuSEXP, SEXP s10SEXP, SEXP EtSEXP, SEXP kBSEXP, SEXP T_KSEXP, SEXP DeltaESEXP, SEXP rhopSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    Rcpp::traits::input_parameter< double >::type Eu(EuSEXP);
    Rcpp::traits::input_parameter< double >::type s10(s10SEXP);
    Rcpp::traits::input_parameter< double >::type Et(EtSEXP);
    Rcpp::traits::input_parameter< double >::type kB(kBSEXP);
    Rcpp::traits::input_parameter< double >::type T_K(T_KSEXP);
    Rcpp::traits::input_parameter< double >::type DeltaE(DeltaESEXP);
    Rcpp::traits::input_parameter< double >::type rhop(rhopSEXP);
    rcpp_result_gen = Rcpp::wrap(f_BTS_cpp_part(x, A, Eu, s10, Et, kB, T_K, DeltaE, rhop));
    return rcpp_result_gen;
END_RCPP
}
// analyse_IRSARRF_SRS
RcppExport SEXP analyse_IRSARRF_SRS(arma::vec values_regenerated_limited, arma::vec values_natural_limited, arma::vec vslide_range, int n_MC, bool trace);
RcppExport SEXP _Luminescence_analyse_IRSARRF_SRS(SEXP values_regenerated_limitedSEXP, SEXP values_natural_limitedSEXP, SEXP vslide_rangeSEXP, SEXP n_MCSEXP, SEXP traceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type values_regenerated_limited(values_regenerated_limitedSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type values_natural_limited(values_natural_limitedSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type vslide_range(vslide_rangeSEXP);
    Rcpp::traits::input_parameter< int >::type n_MC(n_MCSEXP);
    Rcpp::traits::input_parameter< bool >::type trace(traceSEXP);
    rcpp_result_gen = Rcpp::wrap(analyse_IRSARRF_SRS(values_regenerated_limited, values_natural_limited, vslide_range, n_MC, trace));
    return rcpp_result_gen;
END_RCPP
}
// create_RLumDataCurve_matrix
NumericMatrix create_RLumDataCurve_matrix(NumericVector DATA, double VERSION, int NPOINTS, String LTYPE, double LOW, double HIGH, double AN_TEMP, int TOLDELAY, int TOLON, int TOLOFF);
RcppExport SEXP _Luminescence_create_RLumDataCurve_matrix(SEXP DATASEXP, SEXP VERSIONSEXP, SEXP NPOINTSSEXP, SEXP LTYPESEXP, SEXP LOWSEXP, SEXP HIGHSEXP, SEXP AN_TEMPSEXP, SEXP TOLDELAYSEXP, SEXP TOLONSEXP, SEXP TOLOFFSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type DATA(DATASEXP);
    Rcpp::traits::input_parameter< double >::type VERSION(VERSIONSEXP);
    Rcpp::traits::input_parameter< int >::type NPOINTS(NPOINTSSEXP);
    Rcpp::traits::input_parameter< String >::type LTYPE(LTYPESEXP);
    Rcpp::traits::input_parameter< double >::type LOW(LOWSEXP);
    Rcpp::traits::input_parameter< double >::type HIGH(HIGHSEXP);
    Rcpp::traits::input_parameter< double >::type AN_TEMP(AN_TEMPSEXP);
    Rcpp::traits::input_parameter< int >::type TOLDELAY(TOLDELAYSEXP);
    Rcpp::traits::input_parameter< int >::type TOLON(TOLONSEXP);
    Rcpp::traits::input_parameter< int >::type TOLOFF(TOLOFFSEXP);
    rcpp_result_gen = Rcpp::wrap(create_RLumDataCurve_matrix(DATA, VERSION, NPOINTS, LTYPE, LOW, HIGH, AN_TEMP, TOLDELAY, TOLON, TOLOFF));
    return rcpp_result_gen;
END_RCPP
}
// fit_functionEXP_cpp
NumericVector fit_functionEXP_cpp(double a, double b, double c, NumericVector x);
RcppExport SEXP _Luminescence_fit_functionEXP_cpp(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_functionEXP_cpp(a, b, c, x));
    return rcpp_result_gen;
END_RCPP
}
// fit_functionEXPLIN_cpp
NumericVector fit_functionEXPLIN_cpp(double a, double b, double c, double g, NumericVector x);
RcppExport SEXP _Luminescence_fit_functionEXPLIN_cpp(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP, SEXP gSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type g(gSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_functionEXPLIN_cpp(a, b, c, g, x));
    return rcpp_result_gen;
END_RCPP
}
// fit_functionEXPEXP_cpp
NumericVector fit_functionEXPEXP_cpp(double a1, double a2, double b1, double b2, NumericVector x);
RcppExport SEXP _Luminescence_fit_functionEXPEXP_cpp(SEXP a1SEXP, SEXP a2SEXP, SEXP b1SEXP, SEXP b2SEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type a1(a1SEXP);
    Rcpp::traits::input_parameter< double >::type a2(a2SEXP);
    Rcpp::traits::input_parameter< double >::type b1(b1SEXP);
    Rcpp::traits::input_parameter< double >::type b2(b2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_functionEXPEXP_cpp(a1, a2, b1, b2, x));
    return rcpp_result_gen;
END_RCPP
}
// fit_functionGOK_cpp
NumericVector fit_functionGOK_cpp(double a, double b, double c, double d, NumericVector x);
RcppExport SEXP _Luminescence_fit_functionGOK_cpp(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP, SEXP dSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type d(dSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_functionGOK_cpp(a, b, c, d, x));
    return rcpp_result_gen;
END_RCPP
}
// src_get_XSYG_curve_values
NumericMatrix src_get_XSYG_curve_values(std::string s);
RcppExport SEXP _Luminescence_src_get_XSYG_curve_values(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(src_get_XSYG_curve_values(s));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Luminescence_create_UID", (DL_FUNC) &_Luminescence_create_UID, 0},
    {"_Luminescence_src_EED_Calc_Overall_StatUncertainty", (DL_FUNC) &_Luminescence_src_EED_Calc_Overall_StatUncertainty, 4},
    {"_Luminescence_f_BTS_cpp_part", (DL_FUNC) &_Luminescence_f_BTS_cpp_part, 9},
    {"_Luminescence_analyse_IRSARRF_SRS", (DL_FUNC) &_Luminescence_analyse_IRSARRF_SRS, 5},
    {"_Luminescence_create_RLumDataCurve_matrix", (DL_FUNC) &_Luminescence_create_RLumDataCurve_matrix, 10},
    {"_Luminescence_fit_functionEXP_cpp", (DL_FUNC) &_Luminescence_fit_functionEXP_cpp, 4},
    {"_Luminescence_fit_functionEXPLIN_cpp", (DL_FUNC) &_Luminescence_fit_functionEXPLIN_cpp, 5},
    {"_Luminescence_fit_functionEXPEXP_cpp", (DL_FUNC) &_Luminescence_fit_functionEXPEXP_cpp, 5},
    {"_Luminescence_fit_functionGOK_cpp", (DL_FUNC) &_Luminescence_fit_functionGOK_cpp, 5},
    {"_Luminescence_src_get_XSYG_curve_values", (DL_FUNC) &_Luminescence_src_get_XSYG_curve_values, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_Luminescence(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
