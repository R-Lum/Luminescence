/* DO NOT CHANGE MANUALLY! */
/* This file was produced by the function RLum.BuildScripts/RLum.PBS_EntryPointRegistration.R */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _Luminescence_analyse_IRSARRF_SRS(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _Luminescence_create_RLumDataCurve_matrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _Luminescence_create_UID();
extern SEXP _Luminescence_src_get_XSYG_curve_values(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_Luminescence_analyse_IRSARRF_SRS",         (DL_FUNC) &_Luminescence_analyse_IRSARRF_SRS,          5},
    {"_Luminescence_create_RLumDataCurve_matrix", (DL_FUNC) &_Luminescence_create_RLumDataCurve_matrix, 10},
    {"_Luminescence_create_UID",                  (DL_FUNC) &_Luminescence_create_UID,                   0},
    {"_Luminescence_src_get_XSYG_curve_values",   (DL_FUNC) &_Luminescence_src_get_XSYG_curve_values,    1},
    {NULL, NULL, 0}
};

void R_init_Luminescence(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
