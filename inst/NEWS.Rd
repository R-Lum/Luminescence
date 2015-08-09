\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.4.5 (10th August, 2015)}{

  \subsection{Changes S4-objects, methods, classes}{
   \itemize{
      \item With the (internal) switch from inlinedocs to roxygen2 used for the package manual compilation,
      some help page entries for our S4-methods got unintentionally lost. Unfortunately this
      remained unnoticed until the package was already on CRAN. This issue has been corrected
      by picking up some code changes we already made for the package version 0.5.0. However,
      this may become confusing in terms of warning messages ('deprecated') appearing by using object specific
      'get' or 'set' functions such as \code{get_RLum.Analysis}. The function calls are still working, but will
      be defunct in version 0.5.0 and removed in version 0.6.0 or better to say become part
      of more general calls like \code{get_RLum}, \code{set_RLum} and so on.
      With this update in fact this switch has already been made, but the 'old' functions
      are still supported and the warning messages may point on future necessary changes in your scripts.
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

    \item \code{Risoe.BINfileData}: class definition was updated according to changes made in
    the R version 3.0.0 (without effect for the users).

    \item All \code{RLum} classes have been updated according to the changes in R version 3.0.0.

    \item undefined error using \code{get_RLum(..., keep.object = TRUE)} for \code{RLum.Analysis}
    objects are corrected (#233).

    \item argument \code{recursive} added to \code{get_RLum} method for \code{RLum.Analysis} class. This
    should prevent some loop related problems in cases \code{get_RLum} returns a single RLum object instead
    a list of RLum objects
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
          \item argument \code{fit.MC.runs} and \code{slide.MC.runs} replaced by \code{n.MC} (valid for both
          methods),
          \item argument \code{slide.trend.corr} has been removed and with this the underlying
          code, however, a trend is still calculated and used as rejection criteria,
          \item argument \code{legend.pos} moved to the \code{...} argument,
          \item function output \code{class} containing the original function call
          \item re-design and unify the function output by removing and adding output objects,
          see manual for further details,
          \item the way the error is calculated and returned has been changed (see manual for details),
          \item add argument \code{txtProgressBar} (logical),
          \item argument \code{slide.show.density} renamed to \code{slide.show_density},
          \item implement rejection criteria (\code{rejection.criteria}, details see manual),
          \item minor graphical adjustments,
          \item a lot of internal code polishing.

      }

     \item \code{calc_CentralDose()}

        \itemize{

          \item Fix error caused by zero value, function now returns \code{NA} instead of
          crashing (#238).

      }


     \item \code{calc_SourceDoseRate()}

        \itemize{

          \item The arguments \code{measurement.date} and \code{calib.date} no accept also input objects
          of type 'Date',
          \item if no \code{measurement.date} is provided, it will be set automatically to today with
          a warning,
          \item now vector can be passed to argumement \code{measurement.date} this will be
          treated now a little bit more R like in the sense the output has the same length as the input length
          of the argument \code{measurement.date}.

      }


      \item \code{calc_Statistics()}

        \itemize{

          \item default setting of weights definition changed from 1/s to 1/s^2,
          \item fix an error where weighted statistics cannot be
          calculated due to zero values in the input data.frame
          and the function breaks with unclear error message (#239). This also sloves
          the reported problem #236 for \code{plot_KDE} and \code{plot_Histogram}.

      }

      \item \code{fit_CWCurve()}

        \itemize{

          \item fixed a bug that prevented error calculation when using the Levenberg-Marquardt algorithm.

      }


      \item \code{merge_Risoe.BINfileData()}

        \itemize{

          \item Confusing warning message corrected (#230, Thomas Kolb).

      }

       \item \code{merge_RLum.Data.Curve()}

        \itemize{

          \item Adapt function behaviour for curves with different number of data points, but similar
          channel resolution. Function returns a warning and perform the merging only for channels
          available in all curves.

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

          \item summary content 'in confidence interval' changed to 'in 2 sigma',
          \item function crash caused by internal call calc_Statistics() (#238) was
          fixed (#236).

      }


      \item \code{plot_RadialPlot()}

        \itemize{

          \item summary content 'in confidence interval' changed to 'in 2 sigma',
          \item irritating message corrected for the case zero values had been supplied (#237).

      }

      \item \code{plot_RLum()}

        \itemize{

          \item The function adpopt the capability to handle \code{list} objects containing \code{RLum}.
          This may improve the workflow in several cases.

      }

       \item \code{plot_RLum.Analysis()}

        \itemize{

          \item Fix an error that occurred for the argument combination \code{combine = TRUE}
           and \code{RLum.Analysis} objects that only contain one curve object of each type,

           \item add argument \code{subset} to allow a subset during plotting for a better workflow.
           Arguments will be passed to the function \code{plot_RLum}.

      }


      \item \code{plot_RLum.Data.Curve()}

        \itemize{

          \item NA-test implemented. Nothing is plotted if the curve contains NA-values
          (previous behaviour: function stopped with an error).

      }

      \item \code{readBIN2R()}

        \itemize{

          \item Implemnt new logical argument called \code{fastForward}. If \code{TRUE} the output
          is no longer a \code{Risoe.BINfileData} object, but a list of \code{RLum.Analysis} objects.
          This should allow a more efficient data processing in particular cases.

      }

     \item \code{readXSYG2R()}

        \itemize{

          \item The import will not break any longer if the XSYG file is incomplete. Incomplete
          sequences will be removed from the output (incl. terminal feedback), \
          \item new logical argument \code{fastForward} implemented for a more efficient data analysis
          (same functionality as for the function \code{readBIN2R()}.
          If set to \code{TRUE} only a list of \code{RLum.Analysis} objects is returned, instead of
          a list of headers and \code{RLum.Analysis} objects.

      }


     \item \code{Risoe.BINfileData2RLum.Data.Curve()}

        \itemize{

          \item Corrupt RisoeBINfileData-objects containing no measurement data (NPOINTS = 0)
          no longer cause an error during while the RLum.Data.Curve-object is produced. Curve
          x- and y-values are set to NA (BIN-file issue Norbert Mercier).

      }

      \item \code{Second2Gray()}

        \itemize{

          \item The data provided with the argument \code{dose.rate} can be now also of type
          \code{data.frame}

      }


      \item \code{writeR2BIN()}

        \itemize{

          \item Fix some minor issues for if only empty text fields (e.g., COMMENT) are provided,
          \item implement a function stop for the really rare case that someone tries to export
          a 'Risoe.BINfileData-class' object importet in the workspace with an old package version and
          now try to write it back to a BIN-file (e.g., the package example data).

      }

     }
  }

   \subsection{Example data}{
   \itemize{
        \itemize{
        \item The \code{Risoe.BINfileData-class} objects in \code{ExampleData.BINfileData} did not contain
        a \code{.RESERVED} slot for historical reasons.
        So far this caused no problem, but if one wanted to write the BIN-file
        information back to a file an error occurred; this has been fixed now,
        \item fix some internal package related example data size issues (no effect for the users).
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
          \item add import for package 'assertive' to hopefully provide more user friendly error messages,
          \item add import for package 'digest' to generate unique ids for data sets.

        }
   }
 }

}
