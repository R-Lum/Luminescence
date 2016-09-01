\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.2 (1st September 2016)}{



  \subsection{Bugfixes and changes}{
   \itemize{


      \item \code{analyse_IRSAR.RF()}

       \itemize{
         \item Test parameter \code{curves_bounds} was not calculated correctly; fixed,

         \item first output object renamed to \code{data}, instead of \code{De.values}.

       }



      \item \code{analyse_SAR.CWOSL()}

       \itemize{
         \item The first object in the returned \code{RLum.Results} objects was renamed to \code{data}
         to gain consistency with the other functions.

       }

      \item \code{analyse_SAR.TL()}

       \itemize{
         \item The first object in the returned \code{RLum.Results} objects was renamed to \code{data}
         to gain consistency with the other functions.

       }



      \item \code{analyse_pIRIRSequence()}

       \itemize{
         \item The first object in the returned \code{RLum.Results} objects was renamed to \code{data}
         to gain consistency with the other functions.

       }

       \item \code{calc_FadingCorr()}

       \itemize{
         \item New argument \code{tc.g_value} introduced allowing to specify the tc value of the g-value.
         This might be useful with regard to the common normalisation to two days. Internally the function
         re-calculates the g-value to the tc of the De measurement.
         \item new argument \code{verbose} introduced,
         \item documention and examples extended.

       }

     \item \code{plot_AbanicoPlot()}

       \itemize{

         \item Now returns the \code{par} settings and a numerical output is set to default,
         this allows to reproduce the plot and add further information to it,
         \item examples updated.
    }


    \item \code{plot_DRT()}

       \itemize{
         \item Fix statistical summay keyword issue (reported by Alice Versendaal),
         \item improve customised colouring and pch settings.

       }

    \item \code{plot_GrowthCurve()}

       \itemize{
         \item \code{fit.method} set to linear for problematic cases (two reg points).

       }


    \item \code{plot_Histogram()}

       \itemize{

         \item Fix a problem while dealing with \code{RLum.Results} objects as input (#7, thanks
         to Alice Versendaal).
       }

      \item \code{plot_KDE()}

       \itemize{

         \item Fix a problem while dealing with \code{RLum.Results} objects as input (#7, thanks
         to Alice Versendaal),

         \item function will not break any longe if only one point is provided, but in this case
         no density curve is returned.


       }



      }

    }


}
