// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   src_create_RLumDataCurve_matrix()
// Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
// Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// Version: 0.1.0 [2016-06-28]
// Purpose: Function to create the RLum.Data.Curve() matrix ... faster than in R itself
//  - Mainly used by the function Risoe.BINfileData2RLum.Data.Curve()
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <Rcpp.h>
using namespace Rcpp;

// -----------------------------------------------------------------------------------------------
// Define own function to create a sequence for the x-axis
// .. but we do not export them to avoid side effects, as this function is not the same as the
// .. base R function seq()
// ..no export
NumericVector seq(int from, int to, double length_out) {

  //set variables
  NumericVector sequence = length_out;
  double by = (to - from) / (length_out  - 1);

  //loop and create sequence
  for (int i=0; i < length_out; ++i){
    if(i == 0){
      sequence[i] = from;

    }else{
      sequence[i] = sequence[i-1] + by;

    }

  }
  return sequence;
}

// -----------------------------------------------------------------------------------------------
// The function we want to export
// [[Rcpp::export("src_create_RLumDataCurve_matrix")]]
NumericMatrix create_RLumDataCurve_matrix(
  NumericVector DATA,
  int VERSION,
  int NPOINTS,
  String LTYPE,
  int LOW,
  int HIGH,
  int AN_TEMP,
  int TOLDELAY,
  int TOLON,
  int TOLOFF

){

  //generate X vectors
  if(NPOINTS > 0){

    //set needed vectors and predefine matrix
    NumericVector X = NPOINTS;
    NumericMatrix curve_matrix(NPOINTS, 2);

    //fill x column for the case we have a TL curve
    if(LTYPE == "TL" && VERSION >= 4){

      //the heating curve consists of three vectors that needed to
      //be combined
      //
      //(A) - the start ramping
      NumericVector heat_ramp_start = seq(LOW,AN_TEMP,TOLDELAY);
      //
      //(B) - the plateau
      //B is simply TOLON
      //
      //(C) - the end ramping
      NumericVector heat_ramp_end = seq(AN_TEMP, HIGH, TOLOFF);

      //set index counters
      int c = 0;

      //fill vector for temperature
      for(int i = 0; i < X.length(); i++){
        if(i < heat_ramp_start.length()){
          X[i] = heat_ramp_start[i];

        }else if(i >= heat_ramp_start.length() && i < heat_ramp_start.length() + TOLON){
          X[i] = AN_TEMP;

        }else if(i >= heat_ramp_start.length() + TOLON){
          X[i] = heat_ramp_end[c];
          c++;

        }

      }


    }else{
      X = seq(LOW, HIGH, NPOINTS);

    }

    //set final matrix
    curve_matrix.column(0) = X;
    curve_matrix.column(1) = DATA;

    return(curve_matrix);

  }else{

    //set final matrix
    NumericMatrix curve_matrix(1, 2);
    curve_matrix(0,0) = NumericVector::get_na();
    curve_matrix(0,1) = NumericVector::get_na();

    return(curve_matrix);

  }

}

