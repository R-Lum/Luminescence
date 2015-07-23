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


