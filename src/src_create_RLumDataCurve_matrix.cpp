// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   src_create_RLumDataCurve_matrix()
// Author:  Sebastian Kreutzer, Geography & Earth Science,Aberystwyth University (United Kingdom)
// Contact: sebastian.kreutzer@aber.ac.uk
// Version: 0.1.3 [2019-09-19]
// Purpose: Function to create the RLum.Data.Curve() matrix ... faster than in R itself
//  - Mainly used by the function Risoe.BINfileData2RLum.Data.Curve()
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <Rcpp.h>
using namespace Rcpp;

// -----------------------------------------------------------------------------------------------
// Define own function to create a sequence for the x-axis
// .. but we do not export them to avoid side effects, as this function is not the same as the
// .. base R function seq()
// .. no export
NumericVector seq_RLum(double from, double to, int length_out) {

  //calculate by
  double by = (to - from) / length_out;

  //set sequence vector and so set the first channel
  NumericVector sequence(length_out, from + by);

  //loop and create sequence
  for (int i = 1; i < length_out; i++)
    sequence[i] = sequence[i-1] + by;

  return sequence;
}

// -----------------------------------------------------------------------------------------------
// The function we want to export
// [[Rcpp::export("src_create_RLumDataCurve_matrix")]]
NumericMatrix create_RLumDataCurve_matrix(
  NumericVector DATA,
  double VERSION,
  int NPOINTS,
  String LTYPE,
  double LOW,
  double HIGH,
  double AN_TEMP,
  int TOLDELAY,
  int TOLON,
  int TOLOFF
){

  //generate X vectors
  if(NPOINTS > 0){

    //set needed vectors and predefine matrix
    NumericVector X(NPOINTS);
    NumericMatrix curve_matrix(NPOINTS,2);

    //fill x column for the case we have a TL curve
    if(LTYPE == "TL" && VERSION >= 4.0){

      //provide a fall-back for non-conform  BIN/BINX-files, otherwise the
      //the TL curves are wrong without having a reason shown
      if (TOLON == 0 && TOLOFF == 0 && TOLDELAY == 0) {
        Rcout << "[src_create_RLumDataCurve_matrix()] BIN/BINX-file non-conform. TL curve may be wrong!\n";
        TOLOFF = NPOINTS;
      }

      //the heating curve consists of three vectors that needed to
      //be combined
      //
      //(A) - the start ramping
      NumericVector heat_ramp_start = seq_RLum(LOW, AN_TEMP, TOLDELAY);
      //
      //(B) - the plateau
      int B_start = heat_ramp_start.length();
      int B_end = B_start + TOLON;
      //
      //(C) - the end ramping
      NumericVector heat_ramp_end = seq_RLum(AN_TEMP, HIGH, TOLOFF);

      //set index counters
      int i, c = 0;

      //fill vector for temperature
      for (i = 0; i < B_start; i++) {
          X[i] = heat_ramp_start[i];
      }
      for (i = B_start; i < B_end; i++) {
          X[i] = AN_TEMP;
      }
      for (i = B_end; i < NPOINTS; i++, c++) {
          X[i] = heat_ramp_end[c];
      }
    }else{
      X = seq_RLum(LOW, HIGH, NPOINTS);
    }

    //set final matrix
    curve_matrix.column(0) = X;
    curve_matrix.column(1) = DATA;

    return(curve_matrix);

  }else{

    //set final matrix for the case NPOINTS <= 0
    //fill this with NA values
    NumericMatrix curve_matrix(1,2);
    curve_matrix(0,0) = NumericVector::get_na();
    curve_matrix(0,1) = NumericVector::get_na();

    return(curve_matrix);
  }
}
