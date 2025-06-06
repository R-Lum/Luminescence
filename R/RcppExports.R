# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

create_UID <- function() {
    .Call(`_Luminescence_create_UID`)
}

src_EED_Calc_Overall_StatUncertainty <- function(M_Simul, Ndata, Nsimul, MinNbSimExp) {
    .Call(`_Luminescence_src_EED_Calc_Overall_StatUncertainty`, M_Simul, Ndata, Nsimul, MinNbSimExp)
}

f_BTS_cpp_part <- function(x, A, Eu, s10, Et, kB, T_K, DeltaE, rhop) {
    .Call(`_Luminescence_f_BTS_cpp_part`, x, A, Eu, s10, Et, kB, T_K, DeltaE, rhop)
}

src_analyse_IRSARRF_SRS <- function(values_regenerated_limited, values_natural_limited, vslide_range, n_MC, trace = FALSE) {
    .Call(`_Luminescence_analyse_IRSARRF_SRS`, values_regenerated_limited, values_natural_limited, vslide_range, n_MC, trace)
}

src_create_RLumDataCurve_matrix <- function(DATA, VERSION, NPOINTS, LTYPE, LOW, HIGH, AN_TEMP, TOLDELAY, TOLON, TOLOFF) {
    .Call(`_Luminescence_create_RLumDataCurve_matrix`, DATA, VERSION, NPOINTS, LTYPE, LOW, HIGH, AN_TEMP, TOLDELAY, TOLON, TOLOFF)
}

fit_functionEXP_cpp <- function(a, b, c, x) {
    .Call(`_Luminescence_fit_functionEXP_cpp`, a, b, c, x)
}

fit_functionEXPLIN_cpp <- function(a, b, c, g, x) {
    .Call(`_Luminescence_fit_functionEXPLIN_cpp`, a, b, c, g, x)
}

fit_functionEXPEXP_cpp <- function(a1, a2, b1, b2, x) {
    .Call(`_Luminescence_fit_functionEXPEXP_cpp`, a1, a2, b1, b2, x)
}

fit_functionGOK_cpp <- function(a, b, c, d, x) {
    .Call(`_Luminescence_fit_functionGOK_cpp`, a, b, c, d, x)
}

src_get_XSYG_curve_values <- function(s) {
    .Call(`_Luminescence_src_get_XSYG_curve_values`, s)
}

