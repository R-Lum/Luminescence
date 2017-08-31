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

        \item \code{convert_RLum2Risoe.BINfileData()}: Function to convert \code{RLum.Analysis} and \code{RLum.Data.Curve}
        objects to \code{Risoe.BINfileData}, which can be exported to BIN/BINX-files using the function \code{write_R2BIN()}.

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

  \item \code{plot_GrowthCurve()}{
        \itemize{
         \item The function now turns \code{Inf} into \code{NA},
         \item the argument \code{na.rm} is defunct and should not be used anymore,
         \item minor code modifications to handle special cases such as input values with a
         \code{data.frame} with no error column,
         \item extent examples by for test data set by Berger & Huntley (1989).

    }
  }

  \item \code{read_BIN2R()}{
        \itemize{
         \item The function unexpectetly dropped are warning for an unused connection; fixed,
         \item if a file link was provided and the argument \code{pattern} was set and error was shown; fixed.


    }
  }



   \item \code{read_XSYG2R()}{
        \itemize{
         \item Argument \code{verbose} added for more consistency with other functions,
         \item the functions now supports the import form files via the internet,
         \item minor modifications to support minimal examples without 'recordType',
         \item give the XSYG-file import a 50\% performance boost.


    }
  }


 }

}

  \subsection{S4 and Internals}{

      \itemize{
        \item The \code{RLum.Analysis} show method failed for \code{RLum.Data.Spectrum} objects; fixed.
        \item Method for \code{get_RLum} with signature \code{NULL} added. Now it returns \code{NULL}
        \item Start supporting RStudio add-ins for more advanced users.
        \item Add fitting test data set by Berger & Huntley (1989) as external data


    }
  }

}
