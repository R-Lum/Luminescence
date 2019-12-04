// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   src_create_RLumDataCurve_matrix()
// Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
// Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
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
NumericVector seq_RLum(double from, double to, double length_out) {

  //calculate by
  double by = (to - from) / length_out;

  //set sequence vector and so set the first channel
  NumericVector sequence(static_cast<int>(length_out), (from + by));

  //loop and create sequence
  for (int i=1; i < static_cast<int>(length_out); i++)
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

      //provide a fallback for non-conform  BIN/BINX-files, otherwise the
      //the TL curves are wrong withouth having a reason.
      if((TOLON == 0) & (TOLOFF == 0) & (TOLDELAY == 0)){
        Rcout << "[src_create_RLumDataCurve_matrix()] BIN/BINX-file non-conform. TL curve may be wrong!\n";
        TOLOFF = NPOINTS;
      }

      //the heating curve consists of three vectors that needed to
      //be combined
      //
      //(A) - the start ramping
      NumericVector heat_ramp_start = seq_RLum(LOW,AN_TEMP,static_cast<double>(TOLDELAY));
      //
      //(B) - the plateau
      //B is simply TOLON
      //
      //(C) - the end ramping
      NumericVector heat_ramp_end = seq_RLum(AN_TEMP, HIGH, static_cast<double>(TOLOFF));

      //set index counters
      int c = 0;

      //fill vector for temperature
      for(int i = 0; i < X.length(); i++){
        if(i < heat_ramp_start.length()){
          X[i] = heat_ramp_start[i];

        }else if(i >= heat_ramp_start.length() && i < heat_ramp_start.length() + static_cast<double>(TOLON)){
          X[i] = AN_TEMP;

        }else if(i >= heat_ramp_start.length() + TOLON){
          X[i] = heat_ramp_end[c];
          c++;
        }
      }
    }else{
      X = seq_RLum(LOW, HIGH, static_cast<double>(NPOINTS));
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
