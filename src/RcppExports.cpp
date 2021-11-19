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
    {"_Luminescence_analyse_IRSARRF_SRS", (DL_FUNC) &_Luminescence_analyse_IRSARRF_SRS, 5},
    {"_Luminescence_create_RLumDataCurve_matrix", (DL_FUNC) &_Luminescence_create_RLumDataCurve_matrix, 10},
    {"_Luminescence_src_get_XSYG_curve_values", (DL_FUNC) &_Luminescence_src_get_XSYG_curve_values, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_Luminescence(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
