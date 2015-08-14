\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.5.0 (, 2015)}{

  \subsection{Changes S4-objects, methods, classes}{
   \itemize{
      \item Extend class definition for \code{RLum}-class by slots \code{originator} and \code{.uid}
      (and with that for all \code{RLum}classes),

      \item the \code{get_RLum} method for the \code{RLum.Results} object returned \code{NULL} if
      nothing was defined for a specific function, corrected. Now, if nothing is defined the first
      object in the slot \code{data} is returned (#223).
   }
  }

  \subsection{New functions}{
   \itemize{
      \item \code{calc_gSGC()} Calculate De value based on the gSGC by Li et al., 2015
      \item \code{plot_NRt()} Creates a Natural/Regenerated signal vs. time (NR(t)) plot as shown in Steffen et al. 2009
       \item \code{read_Daybreak2R()} Imports data prodcued by a Daybreak TL/OSL reader
       \item \code{use_DRAC()} Provides an R interface to
   }
  }

  \subsection{Bugfixes and changes}{
   \itemize{
      \item \code{analyse_SAR.CWOSL()}
      \itemize{

        \item Internal curve selection now works using the \code{get_RLum}. This should give
        a little bit more flexibility and prevent errors with odd curve names,
        \item in the rare case where no recuperation rate can be calculated due to the missing
        R0 point the function stopped with an error. Corrected.

      }


       \item \code{calc_Statistics()}
       \itemize{

        \item Weighted median is now completly removed from the calculation, values is replaced
        by the unweighted median.


      }


     \item \code{fit_LMCurve()}
      \itemize{

        \item Argument \code{output.plot} renamed to \code{plot},
        \item argument \code{output.plotBG} renamed to \code{plot.BG},
        \item arguemtn \code{output.terminal} renamed to \code{verbose},
        \item argument \code{output.path} removed without replacement, corresponding code has been
        removed from the function,
        \item argument \code{cex.global} removed, scaling can be still used via \code{cex}
        \item argument \code{output.terminaladvanced} has been removed without replacement, terminal
        output can just be triggered via \code{verbose},
        \item new argument added \code{fit.method}. Argument works similar as in \code{fit_CWCurve()}
        and allows to use the Levenberg-Marquardt algorithm for fitting (addressing #55)
        \item new return data object added \code{call} containing the original function call.



      }

      \item \code{plot_AbanicoPlot()}
       \itemize{

        \item Further adjustment to deal with zero values (will now be removed automatically)


      }

      \item \code{plot_GrowthCurve()}
       \itemize{

        \item Final fitting and fitting during the MC runs is now done using the Levenberg-Marquardt from the package 'minpack.lm'. This
        increases the fitting stability and the computation step markedly (#5),
        \item output object is enhanced by the \code{call} containing the function call.


      }


     \item \code{plot_RLum()}
      \itemize{

        \item The \code{...} argument \code{main} can be now be of type \code{list} if a \code{list} of
        objects is provided. This allows to set different plot titles for this specific plot option.

      }


      \item \code{plot_RLum.Analysis()}
      \itemize{

        \item The \code{...} argument \code{main} can now be provided as vector if \code{combine = TRUE}.

      }

    \item \code{readBIN2R()}
      \itemize{

        \item Set function on deprecated, new name: \code{read_BIN2R()}

      }


    \item \code{readSPE2R()}
      \itemize{

        \item Set function on deprecated, new name: \code{read_SPE2R()}

      }


      \item \code{readXSYG2R()}
      \itemize{

        \item Set function to deprecated, new name: \code{read_XSYG2R()}

      }

      \item \code{writeR2BIN()}
      \itemize{

        \item Set function to deprecated, new name: \code{write_R2BIN()}

      }

     }
  }

   \subsection{Internals}{
   \itemize{
      \item Re-adding the package 'zoo' in imports,
      \item adding package 'readxl' to imports (used in \code{use_DRAC} to import the XLS file)
      \item adding package 'httr' to imports (used in \code{use_DRAC} to establish the connection)


   }
  }



}


