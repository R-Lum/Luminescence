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

#include <sstream>
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// print a 2-element vector horizontally
std::string fmt_vec(const arma::vec& vec) {
  std::stringstream ss;
  ss << " [" << std::setw(11) << vec[0]
     << ", " << std::setw(11) << vec[1] << "]";
  return ss.str();
}

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

  const size_t nat_size = values_natural_limited.size();
  const size_t res_size = values_regenerated_limited.size() - nat_size;
  const size_t two_size = 2;

  //variables for the algorithm
  int v_length = vslide_range.size();
  int v_index = 0;
  arma::vec results(res_size);
  arma::vec::fixed<two_size> v_leftright; // the virtual vector
  arma::vec::fixed<two_size> t_leftright; // the test points
  arma::vec::fixed<two_size> c_leftright; // the calculation

  // initialise values: at the beginning, the virtual vector includes all
  // points in vslide_range
  v_leftright[0] = 0;
  v_leftright[1] = vslide_range.size() - 1;

  if (v_length > 1) {
    // select the test region to be the central third of the virtual vector
    t_leftright[0] = v_length/3;
    t_leftright[1] = 2 * v_length/3;
  }

  //***TRACE****
  if(trace == true){
    Rcout << "\n\n [:::src_analyse_IRSAR_SRS()]";
    Rcout << "\n\n --- Initialisation ---\n";
    Rcout << "\n >> v_leftright: " << fmt_vec(v_leftright);
    Rcout << "\n >> t_leftright: " << fmt_vec(t_leftright);
    Rcout << "\n\n --- Optimisation --- \n ";
    Rcout << "\n --------------------------------------------------------------------------------------------------------------------------";
    Rcout << "\n  v_length\t\t\tv_leftright\t\t\tc_leftright\t\t\tt_leftright\tabs.offset";
    Rcout << "\n --------------------------------------------------------------------------------------------------------------------------";
  }

  //(1) calculate sum of the squared residuals
  // this will be used to find the best fit of the curves (which is the minimum)

  //start loop
  do {

    for (size_t t = 0; t < two_size; ++t) {

      //HORIZONTAL SLIDING CORE -------------------------------------------------------------(start)

      results.zeros();
      auto curr_vslide_range = vslide_range[t_leftright[t]];

      //slide the curves against each other
      for (unsigned int i = 0u; i < res_size; ++i) {
        // calculate the sum of squared residuals along one curve

        for (unsigned int j = 0u; j < nat_size; ++j) {
          double residual = values_regenerated_limited[j + i] -
            (values_natural_limited[j] + curr_vslide_range);
          results[i] += residual * residual;
        }
      }

      //HORIZONTAL SLIDING CORE ---------------------------------------------------------------(end)
      c_leftright[t] = min(results);
    }

    // compare results and update variables
    auto diff = c_leftright[0] - c_leftright[1];
    if (diff == 0) {
      v_length = 1;
    } else {
      // find on which side the minimum is, the maximum is on the other side
      auto idx_min = diff < 0 ? 0 : 1, idx_max = 1 - idx_min;

      // set index to where the minimum is
      v_index = v_leftright[idx_min];

      // update vector window
      v_leftright[idx_max] = t_leftright[idx_max];

      //update window length
      v_length = v_leftright[1] - v_leftright[0];
    }

    //update test point index
    t_leftright[0] = v_leftright[0] + v_length/3;
    t_leftright[1] = v_leftright[0] + (2 * (v_length/3));

    //***TRACE****
    if(trace == true){
      Rcout <<
        "\n" << std::setw(10) << v_length <<
        "\t" << fmt_vec(v_leftright) <<
        "\t" << fmt_vec(c_leftright) <<
        "\t" << fmt_vec(t_leftright) <<
        "\t" << std::setw(10) << vslide_range[v_index];
    }

  } while (v_length > 1);

  //***TRACE****
  if(trace == true){
    Rcout << "\n ------------------------------------------------------------------------------------------";
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
  arma::vec results_vector_min_MC(n_MC);
  for (int i = 0; i < n_MC; ++i) {
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
