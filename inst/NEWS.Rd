\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.4.5 (, 2015)}{

  \subsection{Changes S4-objects, methods, classes}{
   \itemize{
      \item With the (internal) switch from inlinedocs to roxygen2 for the package manual compilation,
      some help page entries for our S4-methods got unintentionally lost. Unfortunately this
      remained unnoticed until the package was alread on CRAN. This issue has been corrected
      by picking up some code changes we already made for the package version 0.5.0. However,
      this may become confusing in terms of warning messages ('deprecated') appearing by using object specific
      'get' or 'set' functions such as \code{get_RLum.Analysis}. The function calls are still working, but will
      be removed in version 0.5.0 or better to say become part of more general calls like \code{get_RLum},
      \code{set_RLum} and so on. In fact this switch has already been made, but the 'old' functions
      are still supported and the warning messages may point on necessary changes in your scripts.
      Changes in detail:

      \itemize{

      \item Depraceted S4-generics: \code{set_RLum.Analyis}, \code{set_RLum.Data.Curve}, \code{set_RLum.Data.Image},
      \code{set_RLum.Data.Spectrum}, \code{set_RLum.Results}, \code{get_RLum.Analyis}, \code{get_RLum.Data.Curve},
      \code{get_RLum.Data.Image}, \code{get_RLum.Data.Spectrum}, \code{get_RLum.Results}, \code{length_RLum.Analysis},
      \code{get_structure.RLum.Analysis}.

      \item New S4-generics (partly they alreaded existed as plain R functions): \code{get_RLum}, \code{set_RLum},
      \code{length_RLum}, \code{structure_RLum}.

      \item This brings the following changes: \code{set_RLum} now holds a argument \code{class},
      e.g.: \code{set_RLum(class = 'RLum.Results', data = data)}

      }
   }
  }

  \subsection{Bugfixes and changes}{
   \itemize{

     \item \code{analyse_IRSAR.RF()}

        \itemize{

          \item Arguments \code{fit.range.min} and \code{fit.range.max} have been removed and
          replaced by \code{RF_nat.lim} and \code{RF_reg.lim},
          \item the argument \code{slide.outlier.rm} and the underlying algorithm were
          removed from the function as the functionality turned out to be unreliable
          and 'over-coded' for the intended purpose,
          \item argument \code{xlab.unit} removed (usage of \code{xlab} is still possible),
          \item minor graphical adjustments,
          \item a lot of internal code polishing.

      }


      \item \code{calc_Statistics()}

        \itemize{

          \item default setting of weights definition changed from 1/s to 1/s^2

      }


      \item \code{merge_Risoe.BINfileData()}

        \itemize{

          \item Confusing warning message corrected (#230, Thomas Kolb).

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
          \item further clean-up NAMESPACE file,
          \item dependency to package 'zoo' removed for this build, will be added again in 0.5.0,
          \item add import for package 'assertive' to hopefully provide more user friendly error messages.

        }
   }
 }

}


