\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.8.0 ( ,2017)}{

  \subsection{New functions}{
    \itemize{

         \item \code{analyse_Al2O3_ITC()}:
         Function to analyse data determined to correct irradiation times as suggested by
         Kreutzer et al., 2017

       \item \code{analyse_Al2O3_CrossTalk()}:
         Function to analyse data to analyse cross talk measurments as shown in
         Kreutzer et al., 2017

        \item \code{convert_Activity2Concentration()}: converts nuclide activity data (Bq/kg) to
        concentrations (ppm or \%) and vice versa

        \item \code{calc_WodaFuchs2008()}:
        Function to obtain the equivalent dose using
        the approach proposed by Woda and Fuchs, 2008.

    }
  }


  \subsection{Bugfixes and changes}{

  \itemize{

     \item \code{analyse_IRSAR.RF()}{
        \itemize{
           \item Multicore processing now also supports Windows (#47)
           \item Fixed a bug where manually specifying the number of CPU cores
           via 'method.control = list(cores = x)' was not respected
    }
   }

      \item \code{plot_FilterCombinations()}{
        \itemize{
         \item Add interative mode (\code{interative = TRUE})


    }
 }
}

}

}
