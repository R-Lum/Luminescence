// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   src_analyse_IRSARRF_SRS()
// Author:  Sebastian Kreutzer, Geography & Earth Science,Aberystwyth University (United Kingdom)
// Contact: sebastian.kreutzer@aber.ac.uk
// Version: 0.4.0 [2020-08-17]
// Purpose:
//
//    Function calculates the squared residuals for the R function analyse_IRSAR.RF()
//    including MC runs for the obtained minimum. The function allows a horizontal and
//    a vertical sliding of the curve
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export("src_analyse_IRSARRF_SRS")]]
RcppExport SEXP analyse_IRSARRF_SRS(arma::vec values_regenerated_limited,
                                    arma::vec values_natural_limited,
                                    arma::vec vslide_range,
                                    int n_MC,
                                    bool trace = false
){

  //check for the vslide_range()
  if(vslide_range.size() > 1e+07){
    stop("[:::src_analyse_IRSAR_SRS()] 'vslide_range' exceeded maximum size (1e+07)!");
  }


  //pre-define variables
  arma::vec residuals(values_natural_limited.size());
  arma::vec results(values_regenerated_limited.size() - values_natural_limited.size());
  arma::vec results_vector_min_MC(n_MC);


  //variables for the algorithm
  int v_length;
  int v_index;
  arma::vec v_leftright(2); //the virtual vector
  arma::vec t_leftright(2); //the test points
  arma::vec c_leftright(2); //the calculation

  //(1) calculate sum of the squared residuals
  // this will be used to find the best fit of the curves (which is the minimum)

  //initialise values
  v_length = vslide_range.size();
  v_index = 0;

  v_leftright[0] = 0;
  v_leftright[1] = vslide_range.size() - 1;

  if(v_length == 1){
    t_leftright[0] = 0;
    t_leftright[1] = 0;

  }else{
    t_leftright[0] = v_length/3;
    t_leftright[1] = 2 * v_length/3;

  }

  //***TRACE****
  if(trace == true){
    Rcout << "\n\n [:::src_analyse_IRSAR_SRS()]";
    Rcout << "\n\n--- Inititalisation --- \n ";
    Rcout << "\n >> v_leftright: " << v_leftright;
    Rcout << "\n >> t_leftright: " << t_leftright;
    Rcout << "\n\n --- Optimisation --- \n ";
    Rcout << "\n ---------------------------------------------------------------------------------------------------------";
    Rcout << "\n v_length \t\t v_leftright \t\t  c_leftright  \t\t\t\t absolute offset";
    Rcout << "\n ---------------------------------------------------------------------------------------------------------";

  }

  //start loop
  do {

    for (int t=0;t<static_cast<int>(t_leftright.size()); t++){

      //HORIZONTAL SLIDING CORE -------------------------------------------------------------(start)
      //slide the curves against each other
      for (int i=0; i<static_cast<int>(results.size()); ++i){

        //calculate squared residuals along one curve
        for (int j=0; j<static_cast<int>(values_natural_limited.size()); ++j){
          residuals[j] = pow((values_regenerated_limited[j+i] - (values_natural_limited[j] + vslide_range[t_leftright[t]])),2);

        }

        //sum results and fill the results vector
        results[i] = sum(residuals);

      }

      //HORIZONTAL SLIDING CORE ---------------------------------------------------------------(end)
      c_leftright[t] = min(results);


    }
    //compare results and re-initialise variables

    if(c_leftright[0] < c_leftright[1]){
      v_index = v_leftright[0]; //set index to left test index

      //update vector window (the left remains the same)
      v_leftright[1] = t_leftright[1];

      //update window length
      v_length = v_leftright[1] - v_leftright[0];

    }else if (c_leftright[0] > c_leftright[1]){
      v_index = v_leftright[1]; //set index to right test index

      //update vector window (the right remains the same this time)
      v_leftright[0] = t_leftright[0];

      //update window length
      v_length = v_leftright[1] - v_leftright[0];

    }else{
      v_length = 1;

    }

    //update test point index
    t_leftright[0] = v_leftright[0] + v_length/3;
    t_leftright[1] = v_leftright[0] + (2 * (v_length/3));

    //***TRACE****
    if(trace == true){
      Rcout << "\n " << v_length << " \t\t\t " << v_leftright << " \t\t " << c_leftright << " \t\t\t " << vslide_range[v_index];

    }

  } while (v_length > 1);

  //***TRACE****
  if(trace == true){
    Rcout << "\n ---------------------------------------------------------------------------------------------------------";
    Rcout << "\n >> SRS minimum: \t\t " << c_leftright[0];
    Rcout << "\n >> Vertical offset index: \t " << v_index + 1;
    Rcout << "\n >> Vertical offset absolute: \t " << vslide_range[v_index] << "\n\n";

  }

  //(2) error calculation
  //use this values to bootstrap and find minimum values and to account for the variation
  //that may result from this method itself (the minimum lays within a valley of minima)
  //
  //using the obtained sliding vector and the function RcppArmadillo::sample() (which equals the
  //function sample() in R, but faster)
  //http://gallery.rcpp.org/articles/using-the-Rcpp-based-sample-implementation


  //this follows the way described in Frouin et al., 2017 ... still ...
  for (int i=0; i<static_cast<int>(results_vector_min_MC.size()); ++i){
    results_vector_min_MC[i] = min(
      RcppArmadillo::sample(
        results,
        results.size(),
        TRUE,
        NumericVector::create()
      )
    );
  }

  //build list with four elements
  //sliding_vector: the original results_vector (this can be used to reproduced the results in R)
  //sliding_vector_min_index: the index of the minimum, it is later also calculated in R, however, sometimes we may need it directly
  //sliding_vector_min_MC: minimum values based on bootstrapping
  //vslide_index: this is the index where the minimum was identified for the vertical sliding
  //vslide_minimum: return the identified minimum value, this helps to re-run the function, as the
  //algorithm might got trapped in the local minimum
  List results_list;
  results_list["sliding_vector"] = results;
  results_list["sliding_vector_min_index"] = (int)results.index_min() + 1;
  results_list["sliding_vector_min_MC"] = results_vector_min_MC;
  results_list["vslide_index"] = v_index + 1;
  results_list["vslide_minimum"] = c_leftright[0]; //left and right should be similar

  return results_list;
}
