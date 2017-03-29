#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
  Check these declarations against the C/Fortran source code.
*/

  /* .Call calls */
extern SEXP Luminescence_analyse_IRSARRF_SRS(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Luminescence_create_RLumDataCurve_matrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Luminescence_create_UID();

static const R_CallMethodDef CallEntries[] = {
  {"Luminescence_analyse_IRSARRF_SRS",         (DL_FUNC) &Luminescence_analyse_IRSARRF_SRS,          5},
  {"Luminescence_create_RLumDataCurve_matrix", (DL_FUNC) &Luminescence_create_RLumDataCurve_matrix, 10},
  {"Luminescence_create_UID",                  (DL_FUNC) &Luminescence_create_UID,                   0},
  {NULL, NULL, 0}
};

void R_init_Luminescence(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
