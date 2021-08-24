#include <Rcpp.h>
using namespace Rcpp;

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   find_first_larger_value()
// Author:  Sebastian Kreutzer, Geography & Earth Science,Aberystwyth University (United Kingdom)
//          based on an R script written by Pierre Guibert, IRAMAT-CRP2A,
//          Universite Bordeaux Montaigne (France)
// Contact: sebastian.kreutzer@aber.ac.uk
// Version: 0.1.0 [2019-05-15]
// @description: Find the first positions where the values in x are great than in y
//               Where x and y are numerical vectors
//
// @param x input vector for which we test the values
// @param y input vector for which the values of x are tested against
//
// @return: the output is an index vector with the same length as x
//
// @note: this version is ca. 10 times faster than the previous R implementation
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
NumericVector find_first_larger_value(
    NumericVector x,
    NumericVector y
    ) {

    //initialise variable
    int ic = 0;
    int ic_max = y.size() - 1;
    NumericVector cur_mean_id(x.size());

    for (int i = 0; i < x.size(); i++){
      while (x[i] > y[ic] && ic < ic_max)
        ic++;

      //fill vector
      cur_mean_id[i] = ic;

    }

    return(cur_mean_id);

}

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   row_sd()
// Author:  Sebastian Kreutzer, Geography & Earth Science, Aberystwyth University (United Kingdom)
//
// Contact: sebastian.kreutzer@aber.ac.uk
// Version: 0.1.0 [2019-08-27]
// @description: Calculate the row standard deviation of a matrix
//
// @param m input matrix
//
// @return: vector of length m.ncol()
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
NumericVector row_sd(NumericMatrix m){

    //initialise variables
    double tmp_mean;
    double sum = 0;
    int n = m.ncol();
    NumericVector output(m.nrow());

   //calculate standard deviation
   for(int i = 0; i < m.nrow(); i++){
     tmp_mean = mean(m(i,_));

     //inner loop over elements
     for(int j = 0; j < n; j++){
        sum += pow(m(i,j) - tmp_mean, 2.0);

     }

     output(i) = pow(sum / (n - 1), 0.5);
     sum = 0;

   }

  return(output);
}

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Title:   .EED_Calc_Overall_StatUncertainty
// Author:  Sebastian Kreutzer, Geography & Earth Science,Aberystwyth University (United Kingdom) based
// on R code by Pierre Guibert, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
//
// Contact: sebastian.kreutzer@aber.ac.uk
// Version: 0.1.0 [2019-08-27]
// @description: Helper function to calculate the statistical uncertainty, the Rcpp implementation
// here is around 200 times faster than the pure R implementation
//
// @param M_Simul matrix
//
// @param Ndata integer
//
// @param Nsimul integer
//
// param MinNbSimExp integer
//
// @return matrix with two columns containing the M_SimExpResults
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// [[Rcpp::export]]
NumericMatrix src_EED_Calc_Overall_StatUncertainty (
  NumericMatrix M_Simul,
  int Ndata,
  int Nsimul,
  int MinNbSimExp

){

  // initialise variables
  int Nsimexp = int (Nsimul / Ndata);
  NumericMatrix M_SimExpResults(Ndata, 2);
  NumericMatrix M_StoreData(Ndata, Nsimexp);
  NumericVector M_CurSimExp(Ndata);
  NumericVector cur_mean_id(Ndata);
  NumericVector y = M_Simul(_,6);

  if (Nsimexp > MinNbSimExp){

    //start loop
    for(int j = 0; j < Nsimexp; j++){
      //fill vector
      for(int r = 0; r < Ndata; r++)
        M_CurSimExp(r) = M_Simul(r + (Ndata * j) ,2);

      //sort vector
      M_CurSimExp.sort(false);

      //cumulative sum
      NumericVector cur_mean = cumsum(M_CurSimExp);

      //normalise to data
      for(int d = 0; d < Ndata; d++)
        cur_mean(d) = cur_mean(d) / (d + 1);

      //find first larger value
      cur_mean_id = find_first_larger_value(cur_mean, y);

      //store data in matrix
      for(int d = 0; d < Ndata; d++)
        M_StoreData(d,j) = cur_mean(d) - M_Simul(cur_mean_id(d), 8);

    }

    //calculate row means
    M_SimExpResults(_,0) = rowMeans(M_StoreData);

    //calculate the row sd
    M_SimExpResults(_,1) = row_sd(M_StoreData);

  }

  if (Nsimexp < MinNbSimExp)
    Rcout << "Not enough simulations, please increase Nsimul!";

  return M_SimExpResults;
}
