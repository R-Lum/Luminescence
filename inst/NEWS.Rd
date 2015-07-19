\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.4.5 (, 2015)}{

  \subsection{Changes S4-objects, methods, classes}{
   \itemize{
      \item Dummy
   }
  }

  \subsection{Bugfixes and changes}{
   \itemize{

      \item \code{calc_Statistics()}

        \itemize{

          \item default setting of weights definition changed from 1/s to 1/s^2

      }

      \item \code{plot_AbanicoPlot()}
        \itemize{
          \item zlim-definition bug removed,
          \item automatic bandwidth adjustment implemented,
          \item automatic z-axis scaling for zero scatter implemented,
          \item qr (quartile range) set as default option for scatter polygon,
          \item option cinn (confidence intervals) changed to pnn (percentiles),
          \item summary content 'in confidence interval' changed to 'in 2 sigma',
          \item argument central.value renamed to z.0,
          \item argument centrality removed,
          \item argument bar.col added.

        }

       \item \code{plot_KDE()}

        \itemize{

          \item summary content 'in confidence interval' changed to 'in 2 sigma'

      }


      \item \code{plot_RadialPlot()}

        \itemize{

          \item summary content 'in confidence interval' changed to 'in 2 sigma'

      }

      \item \code{plot_RLum.Data.Curve()}

        \itemize{

          \item NA-test implemented. Nothing is plotted if the curve contains NA-values
          (previous behaviour: function stopped with an error).

      }

     \item \code{readXSYG2R()}

        \itemize{

          \item The import will not break any longer if the XSYG file is incomplete. Incomplete
          sequences will be removed from the output (incl. terminal feedback).

      }


     \item \code{Risoe.BINfileData2RLum.Data.Curve()}

        \itemize{

          \item Corrupt RisoeBINfileData-objects containing no measurement data (NPOINTS = 0)
          no longer cause an error during while the RLum.Data.Curve-object is produced. Curve
          x- and y-values are set to NA (BIN-file issue Norbert Mercier).

      }

     }
  }

   \subsection{Internals}{
   \itemize{
        \itemize{
          \item Hopefully fix NAMESPACE issue with 'rgdal' package,
          \item add imports from base packages as requested by CRAN,
          \item further clean-up NAMESPACE file.

        }
   }
 }

}


