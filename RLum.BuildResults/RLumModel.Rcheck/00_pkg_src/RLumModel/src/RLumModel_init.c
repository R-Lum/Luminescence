/* DO NOT CHANGE MANUALLY! */
/* This file was produced by the function RLumModel.BuildScripts/RLumModel.PBS_EntryPointRegistration.R */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP RLumModel_set_ODE_Rcpp(SEXP, SEXP, SEXP);
extern SEXP RLumModel_set_ODE_Rcpp_LM_OSL(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"RLumModel_set_ODE_Rcpp",        (DL_FUNC) &RLumModel_set_ODE_Rcpp,        3},
    {"RLumModel_set_ODE_Rcpp_LM_OSL", (DL_FUNC) &RLumModel_set_ODE_Rcpp_LM_OSL, 3},
    {NULL, NULL, 0}
};

void R_init_RLumModel(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
