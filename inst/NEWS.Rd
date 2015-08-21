\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.5.0 (, 2015)}{

  \subsection{Changes S4-objects, methods, classes}{
   \itemize{
      \item Extend class definition for \code{RLum}-class by slots \code{originator} and \code{.uid}
      (and with that for all \code{RLum}classes),


      \item \code{get_RLum}
      \itemize{
        \item the method for the \code{RLum.Results} object returned \code{NULL} if nothing was
        defined for a specific function, corrected. Now, if nothing is defined the first  object
        in the slot \code{data} is returned (#223),
        \item the method was enhanced to deal with objects of type \code{list} containing only
        \code{RLum} objects. Depending on the \code{RLum} objects in the list the corresponding \code{get_RLum} methods
        are called.



      }

   }
  }

  \subsection{New functions}{
   \itemize{
      \item \code{calc_gSGC()} Calculate De value based on the gSGC by Li et al., 2015
      \item \code{plot_NRt()} Creates a Natural/Regenerated signal vs. time (NR(t)) plot as shown in Steffen et al. 2009
       \item \code{read_Daybreak2R()} Imports ASCII data produced by a Daybreak TL/OSL reader
       \item \code{use_DRAC()} Provides an R interface to the 'Dose Rate Calculator' (DRAC)
       \item \code{template_DRAC()} Creates a DRAC input template (v1.1) to be used for \code{use_DRAC()}
   }
  }

  \subsection{Bugfixes and changes}{
   \itemize{
      \item \code{analyse_SAR.CWOSL()}
      \itemize{

        \item Internal curve selection now works using the \code{get_RLum}. This should give
        a little bit more flexibility and prevent errors with odd curve names,
        \item in the rare case where no recuperation rate can be calculated due to the missing
        R0 point the function stopped with an error. This has been corrected.
        \item function now how to deal with a \code{list} of \code{RLum.Analysis} objects and will
        re-call itself as elements are in the \code{list} (argument \code{object}). In particular
        cases this may lead to a tremendous efficient data analysis process.

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
        \item output object is enhanced by the \code{call} containing the function call,
        \item for the option \code{fit.method = "LIN"} and \code{fit.method = "EXP OR LIN"} weights had been
        not considered; corrected,
        \item add argument \code{fit.force_through_origin}, it does exactly what you may expect from such
        argument (#225, requested by Alice Versendaal),
        \item get rid of some not very informative warning messages if some fitting failed.


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
      \item Adding package 'readxl' to imports (used in \code{use_DRAC} to import the XLS file)
      \item Adding package 'httr' to imports (used in \code{use_DRAC} to establish the connection)
      \item Fixed a bug in \code{RLum.Data.Image} class that most likely caused the 'rgdal' NAMESPACE issue
      \item Added methods for S3 generics \code{`$`, `[<-`, `[[<-`, print, as.data.frame} for objects of class \code{DRAC.list}
      \item Added methods for S3 generics \code{print} for objects of class \code{DRAC.highlights}
      \item Roxygen2: replaced \code{@exportPattern ^[[:alpha:]]+} with with explicit \code{@export} statements to 
      allow better NAMESPACE housekeeping

   }
  }



}


